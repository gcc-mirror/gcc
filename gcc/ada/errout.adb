------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E R R O U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Warning! Error messages can be generated during Gigi processing by direct
--  calls to error message routines, so it is essential that the processing
--  in this body be consistent with the requirements for the Gigi processing
--  environment, and that in particular, no disallowed table expansion is
--  allowed to occur.

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Erroutc;  use Erroutc;
with Fname;    use Fname;
with Gnatvsn;  use Gnatvsn;
with Hostparm; use Hostparm;
with Lib;      use Lib;
with Opt;      use Opt;
with Nlists;   use Nlists;
with Output;   use Output;
with Scans;    use Scans;
with Sem_Aux;  use Sem_Aux;
with Sinput;   use Sinput;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stylesw;  use Stylesw;
with Uname;    use Uname;

package body Errout is

   Errors_Must_Be_Ignored : Boolean := False;
   --  Set to True by procedure Set_Ignore_Errors (True), when calls to error
   --  message procedures should be ignored (when parsing irrelevant text in
   --  sources being preprocessed).

   Finalize_Called : Boolean := False;
   --  Set True if the Finalize routine has been called

   Warn_On_Instance : Boolean;
   --  Flag set true for warning message to be posted on instance

   ------------------------------------
   -- Table of Non-Instance Messages --
   ------------------------------------

   --  This table contains an entry for every error message processed by the
   --  Error_Msg routine that is not posted on generic (or inlined) instance.
   --  As explained in further detail in the Error_Msg procedure body, this
   --  table is used to avoid posting redundant messages on instances.

   type NIM_Record is record
      Msg : String_Ptr;
      Loc : Source_Ptr;
   end record;
   --  Type used to store text and location of one message

   package Non_Instance_Msgs is new Table.Table (
     Table_Component_Type => NIM_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 100,
     Table_Name           => "Non_Instance_Msgs");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Error_Msg_Internal
     (Msg      : String;
      Sptr     : Source_Ptr;
      Optr     : Source_Ptr;
      Msg_Cont : Boolean);
   --  This is the low level routine used to post messages after dealing with
   --  the issue of messages placed on instantiations (which get broken up
   --  into separate calls in Error_Msg). Sptr is the location on which the
   --  flag will be placed in the output. In the case where the flag is on
   --  the template, this points directly to the template, not to one of the
   --  instantiation copies of the template. Optr is the original location
   --  used to flag the error, and this may indeed point to an instantiation
   --  copy. So typically we can see Optr pointing to the template location
   --  in an instantiation copy when Sptr points to the source location of
   --  the actual instantiation (i.e the line with the new). Msg_Cont is
   --  set true if this is a continuation message.

   function No_Warnings (N : Node_Or_Entity_Id) return Boolean;
   --  Determines if warnings should be suppressed for the given node

   function OK_Node (N : Node_Id) return Boolean;
   --  Determines if a node is an OK node to place an error message on (return
   --  True) or if the error message should be suppressed (return False). A
   --  message is suppressed if the node already has an error posted on it,
   --  or if it refers to an Etype that has an error posted on it, or if
   --  it references an Entity that has an error posted on it.

   procedure Output_Source_Line
     (L     : Physical_Line_Number;
      Sfile : Source_File_Index;
      Errs  : Boolean);
   --  Outputs text of source line L, in file S, together with preceding line
   --  number, as described above for Output_Line_Number. The Errs parameter
   --  indicates if there are errors attached to the line, which forces
   --  listing on, even in the presence of pragma List (Off).

   procedure Set_Msg_Insertion_Column;
   --  Handle column number insertion (@ insertion character)

   procedure Set_Msg_Insertion_Node;
   --  Handle node (name from node) insertion (& insertion character)

   procedure Set_Msg_Insertion_Type_Reference (Flag : Source_Ptr);
   --  Handle type reference (right brace insertion character). Flag is the
   --  location of the flag, which is provided for the internal call to
   --  Set_Msg_Insertion_Line_Number,

   procedure Set_Msg_Insertion_Unit_Name (Suffix : Boolean := True);
   --  Handle unit name insertion ($ insertion character). Depending on Boolean
   --  parameter Suffix, (spec) or (body) is appended after the unit name.

   procedure Set_Msg_Node (Node : Node_Id);
   --  Add the sequence of characters for the name associated with the given
   --  node to the current message. For N_Designator, N_Selected_Component,
   --  N_Defining_Program_Unit_Name, and N_Expanded_Name, the Prefix is
   --  included as well.

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr);
   --  Add a sequence of characters to the current message. The characters may
   --  be one of the special insertion characters (see documentation in spec).
   --  Flag is the location at which the error is to be posted, which is used
   --  to determine whether or not the # insertion needs a file name. The
   --  variables Msg_Buffer are set on return Msglen.

   procedure Set_Posted (N : Node_Id);
   --  Sets the Error_Posted flag on the given node, and all its parents
   --  that are subexpressions and then on the parent non-subexpression
   --  construct that contains the original expression (this reduces the
   --  number of cascaded messages). Note that this call only has an effect
   --  for a serious error. For a non-serious error, it has no effect.

   procedure Set_Qualification (N : Nat; E : Entity_Id);
   --  Outputs up to N levels of qualification for the given entity. For
   --  example, the entity A.B.C.D will output B.C. if N = 2.

   function Special_Msg_Delete
     (Msg : String;
      N   : Node_Or_Entity_Id;
      E   : Node_Or_Entity_Id) return Boolean;
   --  This function is called from Error_Msg_NEL, passing the message Msg,
   --  node N on which the error is to be posted, and the entity or node E
   --  to be used for an & insertion in the message if any. The job of this
   --  procedure is to test for certain cascaded messages that we would like
   --  to suppress. If the message is to be suppressed then we return True.
   --  If the message should be generated (the normal case) False is returned.

   procedure Unwind_Internal_Type (Ent : in out Entity_Id);
   --  This procedure is given an entity id for an internal type, i.e. a type
   --  with an internal name. It unwinds the type to try to get to something
   --  reasonably printable, generating prefixes like "subtype of", "access
   --  to", etc along the way in the buffer. The value in Ent on return is the
   --  final name to be printed. Hopefully this is not an internal name, but in
   --  some internal name cases, it is an internal name, and has to be printed
   --  anyway (although in this case the message has been killed if possible).
   --  The global variable Class_Flag is set to True if the resulting entity
   --  should have 'Class appended to its name (see Add_Class procedure), and
   --  is otherwise unchanged.

   procedure VMS_Convert;
   --  This procedure has no effect if called when the host is not OpenVMS. If
   --  the host is indeed OpenVMS, then the error message stored in Msg_Buffer
   --  is scanned for appearances of switch names which need converting to
   --  corresponding VMS qualifier names. See Gnames/Vnames table in Errout
   --  spec for precise definition of the conversion that is performed by this
   --  routine in OpenVMS mode.

   -----------------------
   -- Change_Error_Text --
   -----------------------

   procedure Change_Error_Text (Error_Id : Error_Msg_Id; New_Msg : String) is
      Save_Next : Error_Msg_Id;
      Err_Id    : Error_Msg_Id := Error_Id;

   begin
      Set_Msg_Text (New_Msg, Errors.Table (Error_Id).Sptr);
      Errors.Table (Error_Id).Text := new String'(Msg_Buffer (1 .. Msglen));

      --  If in immediate error message mode, output modified error message now
      --  This is just a bit tricky, because we want to output just a single
      --  message, and the messages we modified is already linked in. We solve
      --  this by temporarily resetting its forward pointer to empty.

      if Debug_Flag_OO then
         Save_Next := Errors.Table (Error_Id).Next;
         Errors.Table (Error_Id).Next := No_Error_Msg;
         Write_Eol;
         Output_Source_Line
           (Errors.Table (Error_Id).Line, Errors.Table (Error_Id).Sfile, True);
         Output_Error_Msgs (Err_Id);
         Errors.Table (Error_Id).Next := Save_Next;
      end if;
   end Change_Error_Text;

   ------------------------
   -- Compilation_Errors --
   ------------------------

   function Compilation_Errors return Boolean is
   begin
      if not Finalize_Called then
         raise Program_Error;

      --  In formal verification mode, errors issued when analyzing code
      --  are not compilation errors, and should not result in exiting with
      --  an error status. These errors are handled in the driver of the
      --  verification process instead.

      elsif GNATprove_Mode and not Frame_Condition_Mode then
         return False;

      else
         return Erroutc.Compilation_Errors;
      end if;
   end Compilation_Errors;

   ---------------
   -- Error_Msg --
   ---------------

   --  Error_Msg posts a flag at the given location, except that if the
   --  Flag_Location points within a generic template and corresponds to an
   --  instantiation of this generic template, then the actual message will be
   --  posted on the generic instantiation, along with additional messages
   --  referencing the generic declaration.

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr) is
      Sindex : Source_File_Index;
      --  Source index for flag location

      Orig_Loc : Source_Ptr;
      --  Original location of Flag_Location (i.e. location in original
      --  template in instantiation case, otherwise unchanged).

   begin
      --  It is a fatal error to issue an error message when scanning from the
      --  internal source buffer (see Sinput for further documentation)

      pragma Assert (Sinput.Source /= Internal_Source_Ptr);

      --  Return if all errors are to be ignored

      if Errors_Must_Be_Ignored then
         return;
      end if;

      --  If we already have messages, and we are trying to place a message at
      --  No_Location or in package Standard, then just ignore the attempt
      --  since we assume that what is happening is some cascaded junk. Note
      --  that this is safe in the sense that proceeding will surely bomb.

      if Flag_Location < First_Source_Ptr
        and then Total_Errors_Detected > 0
      then
         return;
      end if;

      --  Start of processing for new message

      Sindex := Get_Source_File_Index (Flag_Location);
      Test_Style_Warning_Serious_Unconditional_Msg (Msg);
      Orig_Loc := Original_Location (Flag_Location);

      --  If the current location is in an instantiation, the issue arises of
      --  whether to post the message on the template or the instantiation.

      --  The way we decide is to see if we have posted the same message on
      --  the template when we compiled the template (the template is always
      --  compiled before any instantiations). For this purpose, we use a
      --  separate table of messages. The reason we do this is twofold:

      --     First, the messages can get changed by various processing
      --     including the insertion of tokens etc, making it hard to
      --     do the comparison.

      --     Second, we will suppress a warning on a template if it is not in
      --     the current extended source unit. That's reasonable and means we
      --     don't want the warning on the instantiation here either, but it
      --     does mean that the main error table would not in any case include
      --     the message.

      if Flag_Location = Orig_Loc then
         Non_Instance_Msgs.Append ((new String'(Msg), Flag_Location));
         Warn_On_Instance := False;

      --  Here we have an instance message

      else
         --  Delete if debug flag off, and this message duplicates a message
         --  already posted on the corresponding template

         if not Debug_Flag_GG then
            for J in Non_Instance_Msgs.First .. Non_Instance_Msgs.Last loop
               if Msg = Non_Instance_Msgs.Table (J).Msg.all
                 and then Non_Instance_Msgs.Table (J).Loc = Orig_Loc
               then
                  return;
               end if;
            end loop;
         end if;

         --  No duplicate, so error/warning will be posted on instance

         Warn_On_Instance := Is_Warning_Msg;
      end if;

      --  Ignore warning message that is suppressed for this location. Note
      --  that style checks are not considered warning messages for this
      --  purpose.

      if Is_Warning_Msg and then Warnings_Suppressed (Orig_Loc) then
         return;

      --  For style messages, check too many messages so far

      elsif Is_Style_Msg
        and then Maximum_Messages /= 0
        and then Warnings_Detected >= Maximum_Messages
      then
         return;
      end if;

      --  The idea at this stage is that we have two kinds of messages

      --  First, we have those messages that are to be placed as requested at
      --  Flag_Location. This includes messages that have nothing to do with
      --  generics, and also messages placed on generic templates that reflect
      --  an error in the template itself. For such messages we simply call
      --  Error_Msg_Internal to place the message in the requested location.

      if Instantiation (Sindex) = No_Location then
         Error_Msg_Internal (Msg, Flag_Location, Flag_Location, False);
         return;
      end if;

      --  If we are trying to flag an error in an instantiation, we may have
      --  a generic contract violation. What we generate in this case is:

      --     instantiation error at ...
      --     original error message

      --  or

      --     warning: in instantiation at
      --     warning: original warning message

      --  All these messages are posted at the location of the top level
      --  instantiation. If there are nested instantiations, then the
      --  instantiation error message can be repeated, pointing to each
      --  of the relevant instantiations.

      --  Note: the instantiation mechanism is also shared for inlining of
      --  subprogram bodies when front end inlining is done. In this case the
      --  messages have the form:

      --     in inlined body at ...
      --     original error message

      --  or

      --     warning: in inlined body at
      --     warning: original warning message

      --  OK, here we have an instantiation error, and we need to generate the
      --  error on the instantiation, rather than on the template.

      declare
         Actual_Error_Loc : Source_Ptr;
         --  Location of outer level instantiation in instantiation case, or
         --  just a copy of Flag_Location in the normal case. This is the
         --  location where all error messages will actually be posted.

         Save_Error_Msg_Sloc : constant Source_Ptr := Error_Msg_Sloc;
         --  Save possible location set for caller's message. We need to use
         --  Error_Msg_Sloc for the location of the instantiation error but we
         --  have to preserve a possible original value.

         X : Source_File_Index;

         Msg_Cont_Status : Boolean;
         --  Used to label continuation lines in instantiation case with
         --  proper Msg_Cont status.

      begin
         --  Loop to find highest level instantiation, where all error
         --  messages will be placed.

         X := Sindex;
         loop
            Actual_Error_Loc := Instantiation (X);
            X := Get_Source_File_Index (Actual_Error_Loc);
            exit when Instantiation (X) = No_Location;
         end loop;

         --  Since we are generating the messages at the instantiation point in
         --  any case, we do not want the references to the bad lines in the
         --  instance to be annotated with the location of the instantiation.

         Suppress_Instance_Location := True;
         Msg_Cont_Status := False;

         --  Loop to generate instantiation messages

         Error_Msg_Sloc := Flag_Location;
         X := Get_Source_File_Index (Flag_Location);
         while Instantiation (X) /= No_Location loop

            --  Suppress instantiation message on continuation lines

            if Msg (Msg'First) /= '\' then

               --  Case of inlined body

               if Inlined_Body (X) then
                  if Is_Warning_Msg or else Is_Style_Msg then
                     Error_Msg_Internal
                       ("?in inlined body #",
                        Actual_Error_Loc, Flag_Location, Msg_Cont_Status);
                  else
                     Error_Msg_Internal
                       ("error in inlined body #",
                        Actual_Error_Loc, Flag_Location, Msg_Cont_Status);
                  end if;

               --  Case of generic instantiation

               else
                  if Is_Warning_Msg or else Is_Style_Msg then
                     Error_Msg_Internal
                       ("?in instantiation #",
                        Actual_Error_Loc, Flag_Location, Msg_Cont_Status);
                  else
                     Error_Msg_Internal
                       ("instantiation error #",
                        Actual_Error_Loc, Flag_Location, Msg_Cont_Status);
                  end if;
               end if;
            end if;

            Error_Msg_Sloc := Instantiation (X);
            X := Get_Source_File_Index (Error_Msg_Sloc);
            Msg_Cont_Status := True;
         end loop;

         Suppress_Instance_Location := False;
         Error_Msg_Sloc := Save_Error_Msg_Sloc;

         --  Here we output the original message on the outer instantiation

         Error_Msg_Internal
           (Msg, Actual_Error_Loc, Flag_Location, Msg_Cont_Status);
      end;
   end Error_Msg;

   --------------------------------
   -- Error_Msg_Ada_2012_Feature --
   --------------------------------

   procedure Error_Msg_Ada_2012_Feature (Feature : String; Loc : Source_Ptr) is
   begin
      if Ada_Version < Ada_2012 then
         Error_Msg (Feature & " is an Ada 2012 feature", Loc);

         if No (Ada_Version_Pragma) then
            Error_Msg ("\unit must be compiled with -gnat2012 switch", Loc);
         else
            Error_Msg_Sloc := Sloc (Ada_Version_Pragma);
            Error_Msg ("\incompatible with Ada version set#", Loc);
         end if;
      end if;
   end Error_Msg_Ada_2012_Feature;

   ------------------
   -- Error_Msg_AP --
   ------------------

   procedure Error_Msg_AP (Msg : String) is
      S1 : Source_Ptr;
      C  : Character;

   begin
      --  If we had saved the Scan_Ptr value after scanning the previous
      --  token, then we would have exactly the right place for putting
      --  the flag immediately at hand. However, that would add at least
      --  two instructions to a Scan call *just* to service the possibility
      --  of an Error_Msg_AP call. So instead we reconstruct that value.

      --  We have two possibilities, start with Prev_Token_Ptr and skip over
      --  the current token, which is made harder by the possibility that this
      --  token may be in error, or start with Token_Ptr and work backwards.
      --  We used to take the second approach, but it's hard because of
      --  comments, and harder still because things that look like comments
      --  can appear inside strings. So now we take the first approach.

      --  Note: in the case where there is no previous token, Prev_Token_Ptr
      --  is set to Source_First, which is a reasonable position for the
      --  error flag in this situation.

      S1 := Prev_Token_Ptr;
      C := Source (S1);

      --  If the previous token is a string literal, we need a special approach
      --  since there may be white space inside the literal and we don't want
      --  to stop on that white space.

      --  Note: since this is an error recovery issue anyway, it is not worth
      --  worrying about special UTF_32 line terminator characters here.

      if Prev_Token = Tok_String_Literal then
         loop
            S1 := S1 + 1;

            if Source (S1) = C then
               S1 := S1 + 1;
               exit when Source (S1) /= C;
            elsif Source (S1) in Line_Terminator then
               exit;
            end if;
         end loop;

      --  Character literal also needs special handling

      elsif Prev_Token = Tok_Char_Literal then
         S1 := S1 + 3;

      --  Otherwise we search forward for the end of the current token, marked
      --  by a line terminator, white space, a comment symbol or if we bump
      --  into the following token (i.e. the current token).

      --  Again, it is not worth worrying about UTF_32 special line terminator
      --  characters in this context, since this is only for error recovery.

      else
         while Source (S1) not in Line_Terminator
           and then Source (S1) /= ' '
           and then Source (S1) /= ASCII.HT
           and then (Source (S1) /= '-' or else Source (S1 + 1) /= '-')
           and then S1 /= Token_Ptr
         loop
            S1 := S1 + 1;
         end loop;
      end if;

      --  S1 is now set to the location for the flag

      Error_Msg (Msg, S1);
   end Error_Msg_AP;

   ------------------
   -- Error_Msg_BC --
   ------------------

   procedure Error_Msg_BC (Msg : String) is
   begin
      --  If we are at end of file, post the flag after the previous token

      if Token = Tok_EOF then
         Error_Msg_AP (Msg);

      --  If we are at start of file, post the flag at the current token

      elsif Token_Ptr = Source_First (Current_Source_File) then
         Error_Msg_SC (Msg);

      --  If the character before the current token is a space or a horizontal
      --  tab, then we place the flag on this character (in the case of a tab
      --  we would really like to place it in the "last" character of the tab
      --  space, but that it too much trouble to worry about).

      elsif Source (Token_Ptr - 1) = ' '
         or else Source (Token_Ptr - 1) = ASCII.HT
      then
         Error_Msg (Msg, Token_Ptr - 1);

      --  If there is no space or tab before the current token, then there is
      --  no room to place the flag before the token, so we place it on the
      --  token instead (this happens for example at the start of a line).

      else
         Error_Msg (Msg, Token_Ptr);
      end if;
   end Error_Msg_BC;

   -------------------
   -- Error_Msg_CRT --
   -------------------

   procedure Error_Msg_CRT (Feature : String; N : Node_Id) is
      CNRT : constant String := " not allowed in no run time mode";
      CCRT : constant String := " not supported by configuration>";

      S : String (1 .. Feature'Length + 1 + CCRT'Length);
      L : Natural;

   begin
      S (1) := '|';
      S (2 .. Feature'Length + 1) := Feature;
      L := Feature'Length + 2;

      if No_Run_Time_Mode then
         S (L .. L + CNRT'Length - 1) := CNRT;
         L := L + CNRT'Length - 1;

      else pragma Assert (Configurable_Run_Time_Mode);
         S (L .. L + CCRT'Length - 1) := CCRT;
         L := L + CCRT'Length - 1;
      end if;

      Error_Msg_N (S (1 .. L), N);
      Configurable_Run_Time_Violations := Configurable_Run_Time_Violations + 1;
   end Error_Msg_CRT;

   ------------------
   -- Error_Msg_PT --
   ------------------

   procedure Error_Msg_PT (Typ : Node_Id; Subp : Node_Id) is
   begin
      --  Error message below needs rewording (remember comma in -gnatj
      --  mode) ???

      Error_Msg_NE
        ("first formal of & must be of mode `OUT`, `IN OUT` or " &
         "access-to-variable", Typ, Subp);
      Error_Msg_N
        ("\in order to be overridden by protected procedure or entry " &
         "(RM 9.4(11.9/2))", Typ);
   end Error_Msg_PT;

   -----------------
   -- Error_Msg_F --
   -----------------

   procedure Error_Msg_F (Msg : String; N : Node_Id) is
   begin
      Error_Msg_NEL (Msg, N, N, Sloc (First_Node (N)));
   end Error_Msg_F;

   ------------------
   -- Error_Msg_FE --
   ------------------

   procedure Error_Msg_FE
     (Msg : String;
      N   : Node_Id;
      E   : Node_Or_Entity_Id)
   is
   begin
      Error_Msg_NEL (Msg, N, E, Sloc (First_Node (N)));
   end Error_Msg_FE;

   ------------------------
   -- Error_Msg_Internal --
   ------------------------

   procedure Error_Msg_Internal
     (Msg      : String;
      Sptr     : Source_Ptr;
      Optr     : Source_Ptr;
      Msg_Cont : Boolean)
   is
      Next_Msg : Error_Msg_Id;
      --  Pointer to next message at insertion point

      Prev_Msg : Error_Msg_Id;
      --  Pointer to previous message at insertion point

      Temp_Msg : Error_Msg_Id;

      procedure Handle_Serious_Error;
      --  Internal procedure to do all error message handling for a serious
      --  error message, other than bumping the error counts and arranging
      --  for the message to be output.

      --------------------------
      -- Handle_Serious_Error --
      --------------------------

      procedure Handle_Serious_Error is
      begin
         --  Turn off code generation if not done already

         if Operating_Mode = Generate_Code then
            Operating_Mode := Check_Semantics;
            Expander_Active := False;
         end if;

         --  Set the fatal error flag in the unit table unless we are in
         --  Try_Semantics mode. This stops the semantics from being performed
         --  if we find a serious error. This is skipped if we are currently
         --  dealing with the configuration pragma file.

         if not Try_Semantics and then Current_Source_Unit /= No_Unit then
            Set_Fatal_Error (Get_Source_Unit (Sptr));
         end if;
      end Handle_Serious_Error;

   --  Start of processing for Error_Msg_Internal

   begin
      if Raise_Exception_On_Error /= 0 then
         raise Error_Msg_Exception;
      end if;

      Continuation := Msg_Cont;
      Continuation_New_Line := False;
      Suppress_Message := False;
      Kill_Message := False;
      Set_Msg_Text (Msg, Sptr);

      --  Kill continuation if parent message killed

      if Continuation and Last_Killed then
         return;
      end if;

      --  Return without doing anything if message is suppressed

      if Suppress_Message
        and then not All_Errors_Mode
        and then not Is_Warning_Msg
        and then not Is_Unconditional_Msg
      then
         if not Continuation then
            Last_Killed := True;
         end if;

         return;
      end if;

      --  Return without doing anything if message is killed and this is not
      --  the first error message. The philosophy is that if we get a weird
      --  error message and we already have had a message, then we hope the
      --  weird message is a junk cascaded message

      if Kill_Message
        and then not All_Errors_Mode
        and then Total_Errors_Detected /= 0
      then
         if not Continuation then
            Last_Killed := True;
         end if;

         return;
      end if;

      --  Special check for warning message to see if it should be output

      if Is_Warning_Msg then

         --  Immediate return if warning message and warnings are suppressed

         if Warnings_Suppressed (Optr) or else Warnings_Suppressed (Sptr) then
            Cur_Msg := No_Error_Msg;
            return;
         end if;

         --  If the flag location is in the main extended source unit then for
         --  sure we want the warning since it definitely belongs

         if In_Extended_Main_Source_Unit (Sptr) then
            null;

         --  If the main unit has not been read yet. the warning must be on
         --  a configuration file: gnat.adc or user-defined. This means we
         --  are not parsing the main unit yet, so skip following checks.

         elsif No (Cunit (Main_Unit)) then
            null;

         --  If the flag location is not in the main extended source unit, then
         --  we want to eliminate the warning, unless it is in the extended
         --  main code unit and we want warnings on the instance.

         elsif In_Extended_Main_Code_Unit (Sptr) and then Warn_On_Instance then
            null;

         --  Keep warning if debug flag G set

         elsif Debug_Flag_GG then
            null;

         --  Keep warning if message text contains !!

         elsif Has_Double_Exclam then
            null;

         --  Here is where we delete a warning from a with'ed unit

         else
            Cur_Msg := No_Error_Msg;

            if not Continuation then
               Last_Killed := True;
            end if;

            return;
         end if;
      end if;

      --  If message is to be ignored in special ignore message mode, this is
      --  where we do this special processing, bypassing message output.

      if Ignore_Errors_Enable > 0 then
         if Is_Serious_Error then
            Handle_Serious_Error;
         end if;

         return;
      end if;

      --  If error message line length set, and this is a continuation message
      --  then all we do is to append the text to the text of the last message
      --  with a comma space separator (eliminating a possible (style) or
      --  info prefix).

      if Error_Msg_Line_Length /= 0 and then Continuation then
         Cur_Msg := Errors.Last;

         declare
            Oldm : String_Ptr := Errors.Table (Cur_Msg).Text;
            Newm : String (1 .. Oldm'Last + 2 + Msglen);
            Newl : Natural;
            M    : Natural;

         begin
            --  First copy old message to new one and free it

            Newm (Oldm'Range) := Oldm.all;
            Newl := Oldm'Length;
            Free (Oldm);

            --  Remove (style) or info: at start of message

            if Msglen > 8 and then Msg_Buffer (1 .. 8) = "(style) " then
               M := 9;

            elsif Msglen > 6 and then Msg_Buffer (1 .. 6) = "info: " then
               M := 7;

            else
               M := 1;
            end if;

            --  Now deal with separation between messages. Normally this is
            --  simply comma space, but there are some special cases.

            --  If continuation new line, then put actual NL character in msg

            if Continuation_New_Line then
               Newl := Newl + 1;
               Newm (Newl) := ASCII.LF;

            --  If continuation message is enclosed in parentheses, then
            --  special treatment (don't need a comma, and we want to combine
            --  successive parenthetical remarks into a single one with
            --  separating commas).

            elsif Msg_Buffer (M) = '(' and then Msg_Buffer (Msglen) = ')' then

               --  Case where existing message ends in right paren, remove
               --  and separate parenthetical remarks with a comma.

               if Newm (Newl) = ')' then
                  Newm (Newl) := ',';
                  Msg_Buffer (M) := ' ';

               --  Case where we are adding new parenthetical comment

               else
                  Newl := Newl + 1;
                  Newm (Newl) := ' ';
               end if;

            --  Case where continuation not in parens and no new line

            else
               Newm (Newl + 1 .. Newl + 2) := ", ";
               Newl := Newl + 2;
            end if;

            --  Append new message

            Newm (Newl + 1 .. Newl + Msglen - M + 1) :=
              Msg_Buffer (M .. Msglen);
            Newl := Newl + Msglen - M + 1;
            Errors.Table (Cur_Msg).Text := new String'(Newm (1 .. Newl));

            --  Update warning msg flag and message doc char if needed

            if Is_Warning_Msg then
               if not Errors.Table (Cur_Msg).Warn then
                  Errors.Table (Cur_Msg).Warn := True;
                  Errors.Table (Cur_Msg).Warn_Chr := Warning_Msg_Char;

               elsif Warning_Msg_Char /= ' ' then
                  Errors.Table (Cur_Msg).Warn_Chr := Warning_Msg_Char;
               end if;
            end if;
         end;

         return;
      end if;

      --  Here we build a new error object

      Errors.Append
        ((Text     => new String'(Msg_Buffer (1 .. Msglen)),
          Next     => No_Error_Msg,
          Prev     => No_Error_Msg,
          Sptr     => Sptr,
          Optr     => Optr,
          Sfile    => Get_Source_File_Index (Sptr),
          Line     => Get_Physical_Line_Number (Sptr),
          Col      => Get_Column_Number (Sptr),
          Warn     => Is_Warning_Msg,
          Warn_Chr => Warning_Msg_Char,
          Style    => Is_Style_Msg,
          Serious  => Is_Serious_Error,
          Uncond   => Is_Unconditional_Msg,
          Msg_Cont => Continuation,
          Deleted  => False));
      Cur_Msg := Errors.Last;

      --  If immediate errors mode set, output error message now. Also output
      --  now if the -d1 debug flag is set (so node number message comes out
      --  just before actual error message)

      if Debug_Flag_OO or else Debug_Flag_1 then
         Write_Eol;
         Output_Source_Line
           (Errors.Table (Cur_Msg).Line, Errors.Table (Cur_Msg).Sfile, True);
         Temp_Msg := Cur_Msg;
         Output_Error_Msgs (Temp_Msg);

      --  If not in immediate errors mode, then we insert the message in the
      --  error chain for later output by Finalize. The messages are sorted
      --  first by unit (main unit comes first), and within a unit by source
      --  location (earlier flag location first in the chain).

      else
         --  First a quick check, does this belong at the very end of the chain
         --  of error messages. This saves a lot of time in the normal case if
         --  there are lots of messages.

         if Last_Error_Msg /= No_Error_Msg
           and then Errors.Table (Cur_Msg).Sfile =
                    Errors.Table (Last_Error_Msg).Sfile
           and then (Sptr > Errors.Table (Last_Error_Msg).Sptr
                       or else
                          (Sptr = Errors.Table (Last_Error_Msg).Sptr
                             and then
                               Optr > Errors.Table (Last_Error_Msg).Optr))
         then
            Prev_Msg := Last_Error_Msg;
            Next_Msg := No_Error_Msg;

         --  Otherwise do a full sequential search for the insertion point

         else
            Prev_Msg := No_Error_Msg;
            Next_Msg := First_Error_Msg;
            while Next_Msg /= No_Error_Msg loop
               exit when
                 Errors.Table (Cur_Msg).Sfile < Errors.Table (Next_Msg).Sfile;

               if Errors.Table (Cur_Msg).Sfile =
                    Errors.Table (Next_Msg).Sfile
               then
                  exit when Sptr < Errors.Table (Next_Msg).Sptr
                              or else
                                (Sptr = Errors.Table (Next_Msg).Sptr
                                   and then
                                 Optr < Errors.Table (Next_Msg).Optr);
               end if;

               Prev_Msg := Next_Msg;
               Next_Msg := Errors.Table (Next_Msg).Next;
            end loop;
         end if;

         --  Now we insert the new message in the error chain. The insertion
         --  point for the message is after Prev_Msg and before Next_Msg.

         --  The possible insertion point for the new message is after Prev_Msg
         --  and before Next_Msg. However, this is where we do a special check
         --  for redundant parsing messages, defined as messages posted on the
         --  same line. The idea here is that probably such messages are junk
         --  from the parser recovering. In full errors mode, we don't do this
         --  deletion, but otherwise such messages are discarded at this stage.

         if Prev_Msg /= No_Error_Msg
           and then Errors.Table (Prev_Msg).Line =
                                             Errors.Table (Cur_Msg).Line
           and then Errors.Table (Prev_Msg).Sfile =
                                             Errors.Table (Cur_Msg).Sfile
           and then Compiler_State = Parsing
           and then not All_Errors_Mode
         then
            --  Don't delete unconditional messages and at this stage, don't
            --  delete continuation lines (we attempted to delete those earlier
            --  if the parent message was deleted.

            if not Errors.Table (Cur_Msg).Uncond
              and then not Continuation
            then
               --  Don't delete if prev msg is warning and new msg is an error.
               --  This is because we don't want a real error masked by a
               --  warning. In all other cases (that is parse errors for the
               --  same line that are not unconditional) we do delete the
               --  message. This helps to avoid junk extra messages from
               --  cascaded parsing errors

               if not (Errors.Table (Prev_Msg).Warn
                         or else
                       Errors.Table (Prev_Msg).Style)
                 or else
                      (Errors.Table (Cur_Msg).Warn
                         or else
                       Errors.Table (Cur_Msg).Style)
               then
                  --  All tests passed, delete the message by simply returning
                  --  without any further processing.

                  if not Continuation then
                     Last_Killed := True;
                  end if;

                  return;
               end if;
            end if;
         end if;

         --  Come here if message is to be inserted in the error chain

         if not Continuation then
            Last_Killed := False;
         end if;

         if Prev_Msg = No_Error_Msg then
            First_Error_Msg := Cur_Msg;
         else
            Errors.Table (Prev_Msg).Next := Cur_Msg;
         end if;

         Errors.Table (Cur_Msg).Next := Next_Msg;

         if Next_Msg = No_Error_Msg then
            Last_Error_Msg := Cur_Msg;
         end if;
      end if;

      --  Bump appropriate statistics count

      if Errors.Table (Cur_Msg).Warn or else Errors.Table (Cur_Msg).Style then
         Warnings_Detected := Warnings_Detected + 1;

      else
         Total_Errors_Detected := Total_Errors_Detected + 1;

         if Errors.Table (Cur_Msg).Serious then
            Serious_Errors_Detected := Serious_Errors_Detected + 1;
            Handle_Serious_Error;
         end if;
      end if;

      --  If too many warnings turn off warnings

      if Maximum_Messages /= 0 then
         if Warnings_Detected = Maximum_Messages then
            Warning_Mode := Suppress;
         end if;

         --  If too many errors abandon compilation

         if Total_Errors_Detected = Maximum_Messages then
            raise Unrecoverable_Error;
         end if;
      end if;
   end Error_Msg_Internal;

   -----------------
   -- Error_Msg_N --
   -----------------

   procedure Error_Msg_N (Msg : String; N : Node_Or_Entity_Id) is
   begin
      Error_Msg_NEL (Msg, N, N, Sloc (N));
   end Error_Msg_N;

   ------------------
   -- Error_Msg_NE --
   ------------------

   procedure Error_Msg_NE
     (Msg : String;
      N   : Node_Or_Entity_Id;
      E   : Node_Or_Entity_Id)
   is
   begin
      Error_Msg_NEL (Msg, N, E, Sloc (N));
   end Error_Msg_NE;

   -------------------
   -- Error_Msg_NEL --
   -------------------

   procedure Error_Msg_NEL
     (Msg           : String;
      N             : Node_Or_Entity_Id;
      E             : Node_Or_Entity_Id;
      Flag_Location : Source_Ptr)
   is
   begin
      if Special_Msg_Delete (Msg, N, E) then
         return;
      end if;

      Test_Style_Warning_Serious_Unconditional_Msg (Msg);

      --  Special handling for warning messages

      if Is_Warning_Msg then

         --  Suppress if no warnings set for either entity or node

         if No_Warnings (N) or else No_Warnings (E) then

            --  Disable any continuation messages as well

            Last_Killed := True;
            return;
         end if;

         --  Suppress if inside loop that is known to be null or is probably
         --  null (case where loop executes only if invalid values present).
         --  In either case warnings in the loop are likely to be junk.

         declare
            P : Node_Id;

         begin
            P := Parent (N);
            while Present (P) loop
               if Nkind (P) = N_Loop_Statement
                 and then Suppress_Loop_Warnings (P)
               then
                  return;
               end if;

               P := Parent (P);
            end loop;
         end;
      end if;

      --  Test for message to be output

      if All_Errors_Mode
        or else Is_Unconditional_Msg
        or else Is_Warning_Msg
        or else OK_Node (N)
        or else (Msg (Msg'First) = '\' and then not Last_Killed)
      then
         Debug_Output (N);
         Error_Msg_Node_1 := E;
         Error_Msg (Msg, Flag_Location);

      else
         Last_Killed := True;
      end if;

      if not (Is_Warning_Msg or Is_Style_Msg) then
         Set_Posted (N);
      end if;
   end Error_Msg_NEL;

   ------------------
   -- Error_Msg_NW --
   ------------------

   procedure Error_Msg_NW
     (Eflag : Boolean;
      Msg   : String;
      N     : Node_Or_Entity_Id)
   is
   begin
      if Eflag
        and then In_Extended_Main_Source_Unit (N)
        and then Comes_From_Source (N)
      then
         Error_Msg_NEL (Msg, N, N, Sloc (N));
      end if;
   end Error_Msg_NW;

   -----------------
   -- Error_Msg_S --
   -----------------

   procedure Error_Msg_S (Msg : String) is
   begin
      Error_Msg (Msg, Scan_Ptr);
   end Error_Msg_S;

   ------------------
   -- Error_Msg_SC --
   ------------------

   procedure Error_Msg_SC (Msg : String) is
   begin
      --  If we are at end of file, post the flag after the previous token

      if Token = Tok_EOF then
         Error_Msg_AP (Msg);

      --  For all other cases the message is posted at the current token
      --  pointer position

      else
         Error_Msg (Msg, Token_Ptr);
      end if;
   end Error_Msg_SC;

   ------------------
   -- Error_Msg_SP --
   ------------------

   procedure Error_Msg_SP (Msg : String) is
   begin
      --  Note: in the case where there is no previous token, Prev_Token_Ptr
      --  is set to Source_First, which is a reasonable position for the
      --  error flag in this situation

      Error_Msg (Msg, Prev_Token_Ptr);
   end Error_Msg_SP;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Last_Call : Boolean) is
      Cur : Error_Msg_Id;
      Nxt : Error_Msg_Id;
      F   : Error_Msg_Id;

      procedure Delete_Warning (E : Error_Msg_Id);
      --  Delete a message if not already deleted and adjust warning count

      --------------------
      -- Delete_Warning --
      --------------------

      procedure Delete_Warning (E : Error_Msg_Id) is
      begin
         if not Errors.Table (E).Deleted then
            Errors.Table (E).Deleted := True;
            Warnings_Detected := Warnings_Detected - 1;
         end if;
      end Delete_Warning;

   --  Start of message for Finalize

   begin
      --  Set Prev pointers

      Cur := First_Error_Msg;
      while Cur /= No_Error_Msg loop
         Nxt := Errors.Table (Cur).Next;
         exit when Nxt = No_Error_Msg;
         Errors.Table (Nxt).Prev := Cur;
         Cur := Nxt;
      end loop;

      --  Eliminate any duplicated error messages from the list. This is
      --  done after the fact to avoid problems with Change_Error_Text.

      Cur := First_Error_Msg;
      while Cur /= No_Error_Msg loop
         Nxt := Errors.Table (Cur).Next;

         F := Nxt;
         while F /= No_Error_Msg
           and then Errors.Table (F).Sptr = Errors.Table (Cur).Sptr
         loop
            Check_Duplicate_Message (Cur, F);
            F := Errors.Table (F).Next;
         end loop;

         Cur := Nxt;
      end loop;

      --  Mark any messages suppressed by specific warnings as Deleted

      Cur := First_Error_Msg;
      while Cur /= No_Error_Msg loop
         declare
            CE : Error_Msg_Object renames Errors.Table (Cur);

         begin
            if (CE.Warn and not CE.Deleted)
              and then
                (Warning_Specifically_Suppressed (CE.Sptr, CE.Text)
                   or else
                 Warning_Specifically_Suppressed (CE.Optr, CE.Text))
            then
               Delete_Warning (Cur);

               --  If this is a continuation, delete previous messages

               F := Cur;
               while Errors.Table (F).Msg_Cont loop
                  F := Errors.Table (F).Prev;
                  Delete_Warning (F);
               end loop;

               --  Delete any following continuations

               F := Cur;
               loop
                  F := Errors.Table (F).Next;
                  exit when F = No_Error_Msg;
                  exit when not Errors.Table (F).Msg_Cont;
                  Delete_Warning (F);
               end loop;
            end if;
         end;

         Cur := Errors.Table (Cur).Next;
      end loop;

      Finalize_Called := True;

      --  Check consistency of specific warnings (may add warnings). We only
      --  do this on the last call, after all possible warnings are posted.

      if Last_Call then
         Validate_Specific_Warnings (Error_Msg'Access);
      end if;
   end Finalize;

   ----------------
   -- First_Node --
   ----------------

   function First_Node (C : Node_Id) return Node_Id is
      Orig     : constant Node_Id           := Original_Node (C);
      Loc      : constant Source_Ptr        := Sloc (Orig);
      Sfile    : constant Source_File_Index := Get_Source_File_Index (Loc);
      Earliest : Node_Id;
      Eloc     : Source_Ptr;

      function Test_Earlier (N : Node_Id) return Traverse_Result;
      --  Function applied to every node in the construct

      procedure Search_Tree_First is new Traverse_Proc (Test_Earlier);
      --  Create traversal procedure

      ------------------
      -- Test_Earlier --
      ------------------

      function Test_Earlier (N : Node_Id) return Traverse_Result is
         Norig : constant Node_Id    := Original_Node (N);
         Loc   : constant Source_Ptr := Sloc (Norig);

      begin
         --  Check for earlier

         if Loc < Eloc

           --  Ignore nodes with no useful location information

           and then Loc /= Standard_Location
           and then Loc /= No_Location

           --  Ignore nodes from a different file. This ensures against cases
           --  of strange foreign code somehow being present. We don't want
           --  wild placement of messages if that happens.

           and then Get_Source_File_Index (Loc) = Sfile
         then
            Earliest := Norig;
            Eloc     := Loc;
         end if;

         return OK_Orig;
      end Test_Earlier;

   --  Start of processing for First_Node

   begin
      if Nkind (Orig) in N_Subexpr then
         Earliest := Orig;
         Eloc := Loc;
         Search_Tree_First (Orig);
         return Earliest;

      else
         return Orig;
      end if;
   end First_Node;

   ----------------
   -- First_Sloc --
   ----------------

   function First_Sloc (N : Node_Id) return Source_Ptr is
      SI : constant Source_File_Index := Source_Index (Get_Source_Unit (N));
      SF : constant Source_Ptr        := Source_First (SI);
      F  : Node_Id;
      S  : Source_Ptr;

   begin
      F := First_Node (N);
      S := Sloc (F);

      --  The following circuit is a bit subtle. When we have parenthesized
      --  expressions, then the Sloc will not record the location of the paren,
      --  but we would like to post the flag on the paren. So what we do is to
      --  crawl up the tree from the First_Node, adjusting the Sloc value for
      --  any parentheses we know are present. Yes, we know this circuit is not
      --  100% reliable (e.g. because we don't record all possible paren level
      --  values), but this is only for an error message so it is good enough.

      Node_Loop : loop
         Paren_Loop : for J in 1 .. Paren_Count (F) loop

            --  We don't look more than 12 characters behind the current
            --  location, and in any case not past the front of the source.

            Search_Loop : for K in 1 .. 12 loop
               exit Search_Loop when S = SF;

               if Source_Text (SI) (S - 1) = '(' then
                  S := S - 1;
                  exit Search_Loop;

               elsif Source_Text (SI) (S - 1) <= ' ' then
                  S := S - 1;

               else
                  exit Search_Loop;
               end if;
            end loop Search_Loop;
         end loop Paren_Loop;

         exit Node_Loop when F = N;
         F := Parent (F);
         exit Node_Loop when Nkind (F) not in N_Subexpr;
      end loop Node_Loop;

      return S;
   end First_Sloc;

   -----------------------
   -- Get_Ignore_Errors --
   -----------------------

   function Get_Ignore_Errors return Boolean is
   begin
      return Errors_Must_Be_Ignored;
   end Get_Ignore_Errors;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Errors.Init;
      First_Error_Msg := No_Error_Msg;
      Last_Error_Msg := No_Error_Msg;
      Serious_Errors_Detected := 0;
      Total_Errors_Detected := 0;
      Warnings_Detected := 0;
      Cur_Msg := No_Error_Msg;
      List_Pragmas.Init;

      --  Initialize warnings table, if all warnings are suppressed, supply an
      --  initial dummy entry covering all possible source locations.

      Warnings.Init;
      Specific_Warnings.Init;

      if Warning_Mode = Suppress then
         Warnings.Append
           ((Start => Source_Ptr'First, Stop => Source_Ptr'Last));
      end if;
   end Initialize;

   -----------------
   -- No_Warnings --
   -----------------

   function No_Warnings (N : Node_Or_Entity_Id) return Boolean is
   begin
      if Error_Posted (N) then
         return True;

      elsif Nkind (N) in N_Entity and then Has_Warnings_Off (N) then
         return True;

      elsif Is_Entity_Name (N)
        and then Present (Entity (N))
        and then Has_Warnings_Off (Entity (N))
      then
         return True;

      else
         return False;
      end if;
   end No_Warnings;

   -------------
   -- OK_Node --
   -------------

   function OK_Node (N : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (N);

   begin
      if Error_Posted (N) then
         return False;

      elsif K in N_Has_Etype
        and then Present (Etype (N))
        and then Error_Posted (Etype (N))
      then
         return False;

      elsif (K in N_Op
              or else K = N_Attribute_Reference
              or else K = N_Character_Literal
              or else K = N_Expanded_Name
              or else K = N_Identifier
              or else K = N_Operator_Symbol)
        and then Present (Entity (N))
        and then Error_Posted (Entity (N))
      then
         return False;
      else
         return True;
      end if;
   end OK_Node;

   ---------------------
   -- Output_Messages --
   ---------------------

   procedure Output_Messages is
      E        : Error_Msg_Id;
      Err_Flag : Boolean;

      procedure Write_Error_Summary;
      --  Write error summary

      procedure Write_Header (Sfile : Source_File_Index);
      --  Write header line (compiling or checking given file)

      procedure Write_Max_Errors;
      --  Write message if max errors reached

      -------------------------
      -- Write_Error_Summary --
      -------------------------

      procedure Write_Error_Summary is
      begin
         --  Extra blank line if error messages or source listing were output

         if Total_Errors_Detected + Warnings_Detected > 0
           or else Full_List
         then
            Write_Eol;
         end if;

         --  Message giving number of lines read and number of errors detected.
         --  This normally goes to Standard_Output. The exception is when brief
         --  mode is not set, verbose mode (or full list mode) is set, and
         --  there are errors. In this case we send the message to standard
         --  error to make sure that *something* appears on standard error in
         --  an error situation.

         --  Formerly, only the "# errors" suffix was sent to stderr, whereas
         --  "# lines:" appeared on stdout. This caused problems on VMS when
         --  the stdout buffer was flushed, giving an extra line feed after
         --  the prefix.

         if Total_Errors_Detected + Warnings_Detected /= 0
           and then not Brief_Output
           and then (Verbose_Mode or Full_List)
         then
            Set_Standard_Error;
         end if;

         --  Message giving total number of lines. Don't give this message if
         --  the Main_Source line is unknown (this happens in error situations,
         --  e.g. when integrated preprocessing fails).

         if Main_Source_File /= No_Source_File then
            Write_Str (" ");
            Write_Int (Num_Source_Lines (Main_Source_File));

            if Num_Source_Lines (Main_Source_File) = 1 then
               Write_Str (" line: ");
            else
               Write_Str (" lines: ");
            end if;
         end if;

         if Total_Errors_Detected = 0 then
            Write_Str ("No errors");

         elsif Total_Errors_Detected = 1 then
            Write_Str ("1 error");

         else
            Write_Int (Total_Errors_Detected);
            Write_Str (" errors");
         end if;

         if Warnings_Detected /= 0 then
            Write_Str (", ");
            Write_Int (Warnings_Detected);
            Write_Str (" warning");

            if Warnings_Detected /= 1 then
               Write_Char ('s');
            end if;

            if Warning_Mode = Treat_As_Error then
               Write_Str (" (treated as error");

               if Warnings_Detected /= 1 then
                  Write_Char ('s');
               end if;

               Write_Char (')');
            end if;
         end if;

         Write_Eol;
         Set_Standard_Output;
      end Write_Error_Summary;

      ------------------
      -- Write_Header --
      ------------------

      procedure Write_Header (Sfile : Source_File_Index) is
      begin
         if Verbose_Mode or Full_List then
            if Original_Operating_Mode = Generate_Code then
               Write_Str ("Compiling: ");
            else
               Write_Str ("Checking: ");
            end if;

            Write_Name (Full_File_Name (Sfile));

            if not Debug_Flag_7 then
               Write_Str (" (source file time stamp: ");
               Write_Time_Stamp (Sfile);
               Write_Char (')');
            end if;

            Write_Eol;
         end if;
      end Write_Header;

      ----------------------
      -- Write_Max_Errors --
      ----------------------

      procedure Write_Max_Errors is
      begin
         if Maximum_Messages /= 0 then
            if Warnings_Detected >= Maximum_Messages then
               Set_Standard_Error;
               Write_Line ("maximum number of warnings output");
               Write_Line ("any further warnings suppressed");
               Set_Standard_Output;
            end if;

            --  If too many errors print message

            if Total_Errors_Detected >= Maximum_Messages then
               Set_Standard_Error;
               Write_Line ("fatal error: maximum number of errors detected");
               Set_Standard_Output;
            end if;
         end if;
      end Write_Max_Errors;

   --  Start of processing for Output_Messages

   begin
      --  Error if Finalize has not been called

      if not Finalize_Called then
         raise Program_Error;
      end if;

      --  Reset current error source file if the main unit has a pragma
      --  Source_Reference. This ensures outputting the proper name of
      --  the source file in this situation.

      if Main_Source_File = No_Source_File
        or else Num_SRef_Pragmas (Main_Source_File) /= 0
      then
         Current_Error_Source_File := No_Source_File;
      end if;

      --  Brief Error mode

      if Brief_Output or (not Full_List and not Verbose_Mode) then
         Set_Standard_Error;

         E := First_Error_Msg;
         while E /= No_Error_Msg loop
            if not Errors.Table (E).Deleted and then not Debug_Flag_KK then
               if Full_Path_Name_For_Brief_Errors then
                  Write_Name (Full_Ref_Name (Errors.Table (E).Sfile));
               else
                  Write_Name (Reference_Name (Errors.Table (E).Sfile));
               end if;

               Write_Char (':');
               Write_Int (Int (Physical_To_Logical
                                (Errors.Table (E).Line,
                                 Errors.Table (E).Sfile)));
               Write_Char (':');

               if Errors.Table (E).Col < 10 then
                  Write_Char ('0');
               end if;

               Write_Int (Int (Errors.Table (E).Col));
               Write_Str (": ");
               Output_Msg_Text (E);
               Write_Eol;
            end if;

            E := Errors.Table (E).Next;
         end loop;

         Set_Standard_Output;
      end if;

      --  Full source listing case

      if Full_List then
         List_Pragmas_Index := 1;
         List_Pragmas_Mode := True;
         E := First_Error_Msg;

         --  Normal case, to stdout (copyright notice already output)

         if Full_List_File_Name = null then
            if not Debug_Flag_7 then
               Write_Eol;
            end if;

         --  Output to file

         else
            Create_List_File_Access.all (Full_List_File_Name.all);
            Set_Special_Output (Write_List_Info_Access.all'Access);

            --  Write copyright notice to file

            if not Debug_Flag_7 then
               Write_Str ("GNAT ");
               Write_Str (Gnat_Version_String);
               Write_Eol;
               Write_Str ("Copyright 1992-" &
                          Current_Year &
                          ", Free Software Foundation, Inc.");
               Write_Eol;
            end if;
         end if;

         --  First list extended main source file units with errors

         for U in Main_Unit .. Last_Unit loop
            if In_Extended_Main_Source_Unit (Cunit_Entity (U))

              --  If debug flag d.m is set, only the main source is listed

              and then (U = Main_Unit or else not Debug_Flag_Dot_M)

              --  If the unit of the entity does not come from source, it is
              --  an implicit subprogram declaration for a child subprogram.
              --  Do not emit errors for it, they are listed with the body.

              and then
                (No (Cunit_Entity (U))
                   or else Comes_From_Source (Cunit_Entity (U))
                   or else not Is_Subprogram (Cunit_Entity (U)))
            then
               declare
                  Sfile : constant Source_File_Index := Source_Index (U);

               begin
                  Write_Eol;

                  --  Only write the header if Sfile is known

                  if Sfile /= No_Source_File then
                     Write_Header (Sfile);
                     Write_Eol;
                  end if;

                  --  Normally, we don't want an "error messages from file"
                  --  message when listing the entire file, so we set the
                  --  current source file as the current error source file.
                  --  However, the old style of doing things was to list this
                  --  message if pragma Source_Reference is present, even for
                  --  the main unit. Since the purpose of the -gnatd.m switch
                  --  is to duplicate the old behavior, we skip the reset if
                  --  this debug flag is set.

                  if not Debug_Flag_Dot_M then
                     Current_Error_Source_File := Sfile;
                  end if;

                  --  Only output the listing if Sfile is known, to avoid
                  --  crashing the compiler.

                  if Sfile /= No_Source_File then
                     for N in 1 .. Last_Source_Line (Sfile) loop
                        while E /= No_Error_Msg
                          and then Errors.Table (E).Deleted
                        loop
                           E := Errors.Table (E).Next;
                        end loop;

                        Err_Flag :=
                          E /= No_Error_Msg
                          and then Errors.Table (E).Line = N
                          and then Errors.Table (E).Sfile = Sfile;

                        Output_Source_Line (N, Sfile, Err_Flag);

                        if Err_Flag then
                           Output_Error_Msgs (E);

                           if not Debug_Flag_2 then
                              Write_Eol;
                           end if;
                        end if;
                     end loop;
                  end if;
               end;
            end if;
         end loop;

         --  Then output errors, if any, for subsidiary units not in the
         --  main extended unit.

         --  Note: if debug flag d.m set, include errors for any units other
         --  than the main unit in the extended source unit (e.g. spec and
         --  subunits for a body).

         while E /= No_Error_Msg
           and then (not In_Extended_Main_Source_Unit (Errors.Table (E).Sptr)
                       or else
                        (Debug_Flag_Dot_M
                          and then Get_Source_Unit
                                     (Errors.Table (E).Sptr) /= Main_Unit))
         loop
            if Errors.Table (E).Deleted then
               E := Errors.Table (E).Next;

            else
               Write_Eol;
               Output_Source_Line
                 (Errors.Table (E).Line, Errors.Table (E).Sfile, True);
               Output_Error_Msgs (E);
            end if;
         end loop;

         --  If output to file, write extra copy of error summary to the
         --  output file, and then close it.

         if Full_List_File_Name /= null then
            Write_Error_Summary;
            Write_Max_Errors;
            Close_List_File_Access.all;
            Cancel_Special_Output;
         end if;
      end if;

      --  Verbose mode (error lines only with error flags). Normally this is
      --  ignored in full list mode, unless we are listing to a file, in which
      --  case we still generate -gnatv output to standard output.

      if Verbose_Mode
        and then (not Full_List or else Full_List_File_Name /= null)
      then
         Write_Eol;

         --  Output the header only when Main_Source_File is known

         if Main_Source_File /= No_Source_File then
            Write_Header (Main_Source_File);
         end if;

         E := First_Error_Msg;

         --  Loop through error lines

         while E /= No_Error_Msg loop
            if Errors.Table (E).Deleted then
               E := Errors.Table (E).Next;
            else
               Write_Eol;
               Output_Source_Line
                 (Errors.Table (E).Line, Errors.Table (E).Sfile, True);
               Output_Error_Msgs (E);
            end if;
         end loop;
      end if;

      --  Output error summary if verbose or full list mode

      if Verbose_Mode or else Full_List then
         Write_Error_Summary;
      end if;

      Write_Max_Errors;

      if Warning_Mode = Treat_As_Error then
         Total_Errors_Detected := Total_Errors_Detected + Warnings_Detected;
         Warnings_Detected := 0;
      end if;
   end Output_Messages;

   ------------------------
   -- Output_Source_Line --
   ------------------------

   procedure Output_Source_Line
     (L     : Physical_Line_Number;
      Sfile : Source_File_Index;
      Errs  : Boolean)
   is
      S : Source_Ptr;
      C : Character;

      Line_Number_Output : Boolean := False;
      --  Set True once line number is output

      Empty_Line : Boolean := True;
      --  Set False if line includes at least one character

   begin
      if Sfile /= Current_Error_Source_File then
         Write_Str ("==============Error messages for ");

         case Sinput.File_Type (Sfile) is
            when Sinput.Src =>
               Write_Str ("source");

            when Sinput.Config =>
               Write_Str ("configuration pragmas");

            when Sinput.Def =>
               Write_Str ("symbol definition");

            when Sinput.Preproc =>
               Write_Str ("preprocessing data");
         end case;

         Write_Str (" file: ");
         Write_Name (Full_File_Name (Sfile));
         Write_Eol;

         if Num_SRef_Pragmas (Sfile) > 0 then
            Write_Str ("--------------Line numbers from file: ");
            Write_Name (Full_Ref_Name (Sfile));
            Write_Str (" (starting at line ");
            Write_Int (Int (First_Mapped_Line (Sfile)));
            Write_Char (')');
            Write_Eol;
         end if;

         Current_Error_Source_File := Sfile;
      end if;

      if Errs or List_Pragmas_Mode then
         Output_Line_Number (Physical_To_Logical (L, Sfile));
         Line_Number_Output := True;
      end if;

      S := Line_Start (L, Sfile);

      loop
         C := Source_Text (Sfile) (S);
         exit when C = ASCII.LF or else C = ASCII.CR or else C = EOF;

         --  Deal with matching entry in List_Pragmas table

         if Full_List
           and then List_Pragmas_Index <= List_Pragmas.Last
           and then S = List_Pragmas.Table (List_Pragmas_Index).Ploc
         then
            case List_Pragmas.Table (List_Pragmas_Index).Ptyp is
               when Page =>
                  Write_Char (C);

                  --  Ignore if on line with errors so that error flags
                  --  get properly listed with the error line .

                  if not Errs then
                     Write_Char (ASCII.FF);
                  end if;

               when List_On =>
                  List_Pragmas_Mode := True;

                  if not Line_Number_Output then
                     Output_Line_Number (Physical_To_Logical (L, Sfile));
                     Line_Number_Output := True;
                  end if;

                  Write_Char (C);

               when List_Off =>
                  Write_Char (C);
                  List_Pragmas_Mode := False;
            end case;

            List_Pragmas_Index := List_Pragmas_Index + 1;

         --  Normal case (no matching entry in List_Pragmas table)

         else
            if Errs or List_Pragmas_Mode then
               Write_Char (C);
            end if;
         end if;

         Empty_Line := False;
         S := S + 1;
      end loop;

      --  If we have output a source line, then add the line terminator, with
      --  training spaces preserved (so we output the line exactly as input).

      if Line_Number_Output then
         if Empty_Line then
            Write_Eol;
         else
            Write_Eol_Keep_Blanks;
         end if;
      end if;
   end Output_Source_Line;

   -----------------------------
   -- Remove_Warning_Messages --
   -----------------------------

   procedure Remove_Warning_Messages (N : Node_Id) is

      function Check_For_Warning (N : Node_Id) return Traverse_Result;
      --  This function checks one node for a possible warning message

      function Check_All_Warnings is new Traverse_Func (Check_For_Warning);
      --  This defines the traversal operation

      -----------------------
      -- Check_For_Warning --
      -----------------------

      function Check_For_Warning (N : Node_Id) return Traverse_Result is
         Loc : constant Source_Ptr := Sloc (N);
         E   : Error_Msg_Id;

         function To_Be_Removed (E : Error_Msg_Id) return Boolean;
         --  Returns True for a message that is to be removed. Also adjusts
         --  warning count appropriately.

         -------------------
         -- To_Be_Removed --
         -------------------

         function To_Be_Removed (E : Error_Msg_Id) return Boolean is
         begin
            if E /= No_Error_Msg

               --  Don't remove if location does not match

               and then Errors.Table (E).Optr = Loc

               --  Don't remove if not warning/info message. Note that we do
               --  not remove style messages here. They are warning messages
               --  but not ones we want removed in this context.

               and then Errors.Table (E).Warn

               --  Don't remove unconditional messages

               and then not Errors.Table (E).Uncond
            then
               Warnings_Detected := Warnings_Detected - 1;
               return True;

            --  No removal required

            else
               return False;
            end if;
         end To_Be_Removed;

      --  Start of processing for Check_For_Warnings

      begin
         while To_Be_Removed (First_Error_Msg) loop
            First_Error_Msg := Errors.Table (First_Error_Msg).Next;
         end loop;

         if First_Error_Msg = No_Error_Msg then
            Last_Error_Msg := No_Error_Msg;
         end if;

         E := First_Error_Msg;
         while E /= No_Error_Msg loop
            while To_Be_Removed (Errors.Table (E).Next) loop
               Errors.Table (E).Next :=
                 Errors.Table (Errors.Table (E).Next).Next;

               if Errors.Table (E).Next = No_Error_Msg then
                  Last_Error_Msg := E;
               end if;
            end loop;

            E := Errors.Table (E).Next;
         end loop;

         if Nkind (N) = N_Raise_Constraint_Error
           and then Original_Node (N) /= N
           and then No (Condition (N))
         then
            --  Warnings may have been posted on subexpressions of the original
            --  tree. We place the original node back on the tree to remove
            --  those warnings, whose sloc do not match those of any node in
            --  the current tree. Given that we are in unreachable code, this
            --  modification to the tree is harmless.

            declare
               Status : Traverse_Final_Result;

            begin
               if Is_List_Member (N) then
                  Set_Condition (N, Original_Node (N));
                  Status := Check_All_Warnings (Condition (N));
               else
                  Rewrite (N, Original_Node (N));
                  Status := Check_All_Warnings (N);
               end if;

               return Status;
            end;

         else
            return OK;
         end if;
      end Check_For_Warning;

   --  Start of processing for Remove_Warning_Messages

   begin
      if Warnings_Detected /= 0 then
         declare
            Discard : Traverse_Final_Result;
            pragma Warnings (Off, Discard);

         begin
            Discard := Check_All_Warnings (N);
         end;
      end if;
   end Remove_Warning_Messages;

   procedure Remove_Warning_Messages (L : List_Id) is
      Stat : Node_Id;
   begin
      if Is_Non_Empty_List (L) then
         Stat := First (L);
         while Present (Stat) loop
            Remove_Warning_Messages (Stat);
            Next (Stat);
         end loop;
      end if;
   end Remove_Warning_Messages;

   ---------------------------
   -- Set_Identifier_Casing --
   ---------------------------

   procedure Set_Identifier_Casing
     (Identifier_Name : System.Address;
      File_Name       : System.Address)
   is
      Ident : constant Big_String_Ptr := To_Big_String_Ptr (Identifier_Name);
      File  : constant Big_String_Ptr := To_Big_String_Ptr (File_Name);
      Flen  : Natural;

      Desired_Case : Casing_Type := Mixed_Case;
      --  Casing required for result. Default value of Mixed_Case is used if
      --  for some reason we cannot find the right file name in the table.

   begin
      --  Get length of file name

      Flen := 0;
      while File (Flen + 1) /= ASCII.NUL loop
         Flen := Flen + 1;
      end loop;

      --  Loop through file names to find matching one. This is a bit slow, but
      --  we only do it in error situations so it is not so terrible. Note that
      --  if the loop does not exit, then the desired case will be left set to
      --  Mixed_Case, this can happen if the name was not in canonical form,
      --  and gets canonicalized on VMS. Possibly we could fix this by
      --  unconditionally canonicalizing these names ???

      for J in 1 .. Last_Source_File loop
         Get_Name_String (Full_Debug_Name (J));

         if Name_Len = Flen
           and then Name_Buffer (1 .. Name_Len) = String (File (1 .. Flen))
         then
            Desired_Case := Identifier_Casing (J);
            exit;
         end if;
      end loop;

      --  Copy identifier as given to Name_Buffer

      for J in Name_Buffer'Range loop
         Name_Buffer (J) := Ident (J);

         if Name_Buffer (J) = ASCII.NUL then
            Name_Len := J - 1;
            exit;
         end if;
      end loop;

      Set_Casing (Desired_Case);
   end Set_Identifier_Casing;

   -----------------------
   -- Set_Ignore_Errors --
   -----------------------

   procedure Set_Ignore_Errors (To : Boolean) is
   begin
      Errors_Must_Be_Ignored := To;
   end Set_Ignore_Errors;

   ------------------------------
   -- Set_Msg_Insertion_Column --
   ------------------------------

   procedure Set_Msg_Insertion_Column is
   begin
      if RM_Column_Check then
         Set_Msg_Str (" in column ");
         Set_Msg_Int (Int (Error_Msg_Col) + 1);
      end if;
   end Set_Msg_Insertion_Column;

   ----------------------------
   -- Set_Msg_Insertion_Node --
   ----------------------------

   procedure Set_Msg_Insertion_Node is
      K : Node_Kind;

   begin
      Suppress_Message :=
        Error_Msg_Node_1 = Error
          or else Error_Msg_Node_1 = Any_Type;

      if Error_Msg_Node_1 = Empty then
         Set_Msg_Blank_Conditional;
         Set_Msg_Str ("<empty>");

      elsif Error_Msg_Node_1 = Error then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      elsif Error_Msg_Node_1 = Standard_Void_Type then
         Set_Msg_Blank;
         Set_Msg_Str ("procedure name");

      else
         Set_Msg_Blank_Conditional;

         --  Output name

         K := Nkind (Error_Msg_Node_1);

         --  If we have operator case, skip quotes since name of operator
         --  itself will supply the required quotations. An operator can be an
         --  applied use in an expression or an explicit operator symbol, or an
         --  identifier whose name indicates it is an operator.

         if K in N_Op
           or else K = N_Operator_Symbol
           or else K = N_Defining_Operator_Symbol
           or else ((K = N_Identifier or else K = N_Defining_Identifier)
                       and then Is_Operator_Name (Chars (Error_Msg_Node_1)))
         then
            Set_Msg_Node (Error_Msg_Node_1);

         --  Normal case, not an operator, surround with quotes

         else
            Set_Msg_Quote;
            Set_Qualification (Error_Msg_Qual_Level, Error_Msg_Node_1);
            Set_Msg_Node (Error_Msg_Node_1);
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignment ensures that a second ampersand insertion
      --  character will correspond to the Error_Msg_Node_2 parameter. We
      --  suppress possible validity checks in case operating in -gnatVa mode,
      --  and Error_Msg_Node_2 is not needed and has not been set.

      declare
         pragma Suppress (Range_Check);
      begin
         Error_Msg_Node_1 := Error_Msg_Node_2;
      end;
   end Set_Msg_Insertion_Node;

   --------------------------------------
   -- Set_Msg_Insertion_Type_Reference --
   --------------------------------------

   procedure Set_Msg_Insertion_Type_Reference (Flag : Source_Ptr) is
      Ent : Entity_Id;

   begin
      Set_Msg_Blank;

      if Error_Msg_Node_1 = Standard_Void_Type then
         Set_Msg_Str ("package or procedure name");
         return;

      elsif Error_Msg_Node_1 = Standard_Exception_Type then
         Set_Msg_Str ("exception name");
         return;

      elsif     Error_Msg_Node_1 = Any_Access
        or else Error_Msg_Node_1 = Any_Array
        or else Error_Msg_Node_1 = Any_Boolean
        or else Error_Msg_Node_1 = Any_Character
        or else Error_Msg_Node_1 = Any_Composite
        or else Error_Msg_Node_1 = Any_Discrete
        or else Error_Msg_Node_1 = Any_Fixed
        or else Error_Msg_Node_1 = Any_Integer
        or else Error_Msg_Node_1 = Any_Modular
        or else Error_Msg_Node_1 = Any_Numeric
        or else Error_Msg_Node_1 = Any_Real
        or else Error_Msg_Node_1 = Any_Scalar
        or else Error_Msg_Node_1 = Any_String
      then
         Get_Unqualified_Decoded_Name_String (Chars (Error_Msg_Node_1));
         Set_Msg_Name_Buffer;
         return;

      elsif Error_Msg_Node_1 = Universal_Real then
         Set_Msg_Str ("type universal real");
         return;

      elsif Error_Msg_Node_1 = Universal_Integer then
         Set_Msg_Str ("type universal integer");
         return;

      elsif Error_Msg_Node_1 = Universal_Fixed then
         Set_Msg_Str ("type universal fixed");
         return;
      end if;

      --  Special case of anonymous array

      if Nkind (Error_Msg_Node_1) in N_Entity
        and then Is_Array_Type (Error_Msg_Node_1)
        and then Present (Related_Array_Object (Error_Msg_Node_1))
      then
         Set_Msg_Str ("type of ");
         Set_Msg_Node (Related_Array_Object (Error_Msg_Node_1));
         Set_Msg_Str (" declared");
         Set_Msg_Insertion_Line_Number
           (Sloc (Related_Array_Object (Error_Msg_Node_1)), Flag);
         return;
      end if;

      --  If we fall through, it is not a special case, so first output
      --  the name of the type, preceded by private for a private type

      if Is_Private_Type (Error_Msg_Node_1) then
         Set_Msg_Str ("private type ");
      else
         Set_Msg_Str ("type ");
      end if;

      Ent := Error_Msg_Node_1;

      if Is_Internal_Name (Chars (Ent)) then
         Unwind_Internal_Type (Ent);
      end if;

      --  Types in Standard are displayed as "Standard.name"

      if Sloc (Ent) <= Standard_Location then
         Set_Msg_Quote;
         Set_Msg_Str ("Standard.");
         Set_Msg_Node (Ent);
         Add_Class;
         Set_Msg_Quote;

      --  Types in other language defined units are displayed as
      --  "package-name.type-name"

      elsif
        Is_Predefined_File_Name (Unit_File_Name (Get_Source_Unit (Ent)))
      then
         Get_Unqualified_Decoded_Name_String
           (Unit_Name (Get_Source_Unit (Ent)));
         Name_Len := Name_Len - 2;
         Set_Msg_Quote;
         Set_Casing (Mixed_Case);
         Set_Msg_Name_Buffer;
         Set_Msg_Char ('.');
         Set_Casing (Mixed_Case);
         Set_Msg_Node (Ent);
         Add_Class;
         Set_Msg_Quote;

      --  All other types display as "type name" defined at line xxx
      --  possibly qualified if qualification is requested.

      else
         Set_Msg_Quote;
         Set_Qualification (Error_Msg_Qual_Level, Ent);
         Set_Msg_Node (Ent);
         Add_Class;

         --  If Ent is an anonymous subprogram type, there is no name to print,
         --  so remove enclosing quotes.

         if Buffer_Ends_With ("""") then
            Buffer_Remove ("""");
         else
            Set_Msg_Quote;
         end if;
      end if;

      --  If the original type did not come from a predefined file, add the
      --  location where the type was defined.

      if Sloc (Error_Msg_Node_1) > Standard_Location
        and then
          not Is_Predefined_File_Name
                (Unit_File_Name (Get_Source_Unit (Error_Msg_Node_1)))
      then
         Set_Msg_Str (" defined");
         Set_Msg_Insertion_Line_Number (Sloc (Error_Msg_Node_1), Flag);

      --  If it did come from a predefined file, deal with the case where
      --  this was a file with a generic instantiation from elsewhere.

      else
         if Sloc (Error_Msg_Node_1) > Standard_Location then
            declare
               Iloc : constant Source_Ptr :=
                 Instantiation_Location (Sloc (Error_Msg_Node_1));

            begin
               if Iloc /= No_Location
                 and then not Suppress_Instance_Location
               then
                  Set_Msg_Str (" from instance");
                  Set_Msg_Insertion_Line_Number (Iloc, Flag);
               end if;
            end;
         end if;
      end if;
   end Set_Msg_Insertion_Type_Reference;

   ---------------------------------
   -- Set_Msg_Insertion_Unit_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_Unit_Name (Suffix : Boolean := True) is
   begin
      if Error_Msg_Unit_1 = No_Unit_Name then
         null;

      elsif Error_Msg_Unit_1 = Error_Unit_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Get_Unit_Name_String (Error_Msg_Unit_1, Suffix);
         Set_Msg_Blank;
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignment ensures that a second percent insertion
      --  character will correspond to the Error_Msg_Unit_2 parameter. We
      --  suppress possible validity checks in case operating in -gnatVa mode,
      --  and Error_Msg_Unit_2 is not needed and has not been set.

      declare
         pragma Suppress (Range_Check);
      begin
         Error_Msg_Unit_1 := Error_Msg_Unit_2;
      end;
   end Set_Msg_Insertion_Unit_Name;

   ------------------
   -- Set_Msg_Node --
   ------------------

   procedure Set_Msg_Node (Node : Node_Id) is
      Ent : Entity_Id;
      Nam : Name_Id;

   begin
      case Nkind (Node) is
         when N_Designator =>
            Set_Msg_Node (Name (Node));
            Set_Msg_Char ('.');
            Set_Msg_Node (Identifier (Node));
            return;

         when N_Defining_Program_Unit_Name =>
            Set_Msg_Node (Name (Node));
            Set_Msg_Char ('.');
            Set_Msg_Node (Defining_Identifier (Node));
            return;

         when N_Selected_Component | N_Expanded_Name =>
            Set_Msg_Node (Prefix (Node));
            Set_Msg_Char ('.');
            Set_Msg_Node (Selector_Name (Node));
            return;

         when others =>
            null;
      end case;

      --  The only remaining possibilities are identifiers, defining
      --  identifiers, pragmas, and pragma argument associations.

      if Nkind (Node) = N_Pragma then
         Nam := Pragma_Name (Node);

      --  The other cases have Chars fields, and we want to test for possible
      --  internal names, which generally represent something gone wrong. An
      --  exception is the case of internal type names, where we try to find a
      --  reasonable external representation for the external name

      elsif Is_Internal_Name (Chars (Node))
        and then
          ((Is_Entity_Name (Node)
                          and then Present (Entity (Node))
                          and then Is_Type (Entity (Node)))
              or else
           (Nkind (Node) = N_Defining_Identifier and then Is_Type (Node)))
      then
         if Nkind (Node) = N_Identifier then
            Ent := Entity (Node);
         else
            Ent := Node;
         end if;

         --  If the type is the designated type of an access_to_subprogram,
         --  there is no name to provide in the call.

         if Ekind (Ent) = E_Subprogram_Type then
            return;
         else
            Unwind_Internal_Type (Ent);
            Nam := Chars (Ent);
         end if;

      --  If not internal name, just use name in Chars field

      else
         Nam := Chars (Node);
      end if;

      --  At this stage, the name to output is in Nam

      Get_Unqualified_Decoded_Name_String (Nam);

      --  Remove trailing upper case letters from the name (useful for
      --  dealing with some cases of internal names.

      while Name_Len > 1 and then Name_Buffer (Name_Len) in 'A' .. 'Z' loop
         Name_Len := Name_Len  - 1;
      end loop;

      --  If we have any of the names from standard that start with the
      --  characters "any " (e.g. Any_Type), then kill the message since
      --  almost certainly it is a junk cascaded message.

      if Name_Len > 4
        and then Name_Buffer (1 .. 4) = "any "
      then
         Kill_Message := True;
      end if;

      --  Now we have to set the proper case. If we have a source location
      --  then do a check to see if the name in the source is the same name
      --  as the name in the Names table, except for possible differences
      --  in case, which is the case when we can copy from the source.

      declare
         Src_Loc : constant Source_Ptr := Sloc (Node);
         Sbuffer : Source_Buffer_Ptr;
         Ref_Ptr : Integer;
         Src_Ptr : Source_Ptr;

      begin
         Ref_Ptr := 1;
         Src_Ptr := Src_Loc;

         --  For standard locations, always use mixed case

         if Src_Loc <= No_Location
           or else Sloc (Node) <= No_Location
         then
            Set_Casing (Mixed_Case);

         else
            --  Determine if the reference we are dealing with corresponds to
            --  text at the point of the error reference. This will often be
            --  the case for simple identifier references, and is the case
            --  where we can copy the spelling from the source.

            Sbuffer := Source_Text (Get_Source_File_Index (Src_Loc));

            while Ref_Ptr <= Name_Len loop
               exit when
                 Fold_Lower (Sbuffer (Src_Ptr)) /=
                 Fold_Lower (Name_Buffer (Ref_Ptr));
               Ref_Ptr := Ref_Ptr + 1;
               Src_Ptr := Src_Ptr + 1;
            end loop;

            --  If we get through the loop without a mismatch, then output the
            --  name the way it is spelled in the source program

            if Ref_Ptr > Name_Len then
               Src_Ptr := Src_Loc;

               for J in 1 .. Name_Len loop
                  Name_Buffer (J) := Sbuffer (Src_Ptr);
                  Src_Ptr := Src_Ptr + 1;
               end loop;

            --  Otherwise set the casing using the default identifier casing

            else
               Set_Casing (Identifier_Casing (Flag_Source), Mixed_Case);
            end if;
         end if;
      end;

      Set_Msg_Name_Buffer;
      Add_Class;
   end Set_Msg_Node;

   ------------------
   -- Set_Msg_Text --
   ------------------

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr) is
      C : Character;   -- Current character
      P : Natural;     -- Current index;

      procedure Set_Msg_Insertion_Warning;
      --  Deal with ? ?? ?x? ?X? insertion sequences

      -------------------------------
      -- Set_Msg_Insertion_Warning --
      -------------------------------

      procedure Set_Msg_Insertion_Warning is
      begin
         Warning_Msg_Char := ' ';

         if P <= Text'Last and then Text (P) = '?' then
            if Warning_Doc_Switch then
               Warning_Msg_Char := '?';
            end if;

            P := P + 1;

         elsif P + 1 <= Text'Last
           and then (Text (P) in 'a' .. 'z'
                      or else
                     Text (P) in 'A' .. 'Z')
           and then Text (P + 1) = '?'
         then
            if Warning_Doc_Switch then
               Warning_Msg_Char := Text (P);
            end if;

            P := P + 2;
         end if;
      end Set_Msg_Insertion_Warning;

   --  Start of processing for Set_Msg_Text

   begin
      Manual_Quote_Mode := False;
      Msglen := 0;
      Flag_Source := Get_Source_File_Index (Flag);

      P := Text'First;
      while P <= Text'Last loop
         C := Text (P);
         P := P + 1;

         --  Check for insertion character or sequence

         case C is
            when '%' =>
               if P <= Text'Last and then Text (P) = '%' then
                  P := P + 1;
                  Set_Msg_Insertion_Name_Literal;
               else
                  Set_Msg_Insertion_Name;
               end if;

            when '$' =>
               if P <= Text'Last and then Text (P) = '$' then
                  P := P + 1;
                  Set_Msg_Insertion_Unit_Name (Suffix => False);
               else
                  Set_Msg_Insertion_Unit_Name;
               end if;

            when '{' =>
               Set_Msg_Insertion_File_Name;

            when '}' =>
               Set_Msg_Insertion_Type_Reference (Flag);

            when '*' =>
               Set_Msg_Insertion_Reserved_Name;

            when '&' =>
               Set_Msg_Insertion_Node;

            when '#' =>
               Set_Msg_Insertion_Line_Number (Error_Msg_Sloc, Flag);

            when '\' =>
               Continuation := True;

               if Text (P) = '\' then
                  Continuation_New_Line := True;
                  P := P + 1;
               end if;

            when '@' =>
               Set_Msg_Insertion_Column;

            when '>' =>
               Set_Msg_Insertion_Run_Time_Name;

            when '^' =>
               Set_Msg_Insertion_Uint;

            when '`' =>
               Manual_Quote_Mode := not Manual_Quote_Mode;
               Set_Msg_Char ('"');

            when '!' =>
               null; -- already dealt with

            when '?' =>
               Set_Msg_Insertion_Warning;

            when '<' =>

               --  If tagging of messages is enabled, and this is a warning,
               --  then it is treated as being [enabled by default].

               if Error_Msg_Warn
                 and Warning_Doc_Switch
               then
                  Warning_Msg_Char := '?';
               end if;

            when '|' =>
               null; -- already dealt with

            when ''' =>
               Set_Msg_Char (Text (P));
               P := P + 1;

            when '~' =>
               Set_Msg_Str (Error_Msg_String (1 .. Error_Msg_Strlen));

            --  Upper case letter

            when 'A' .. 'Z' =>

               --  Start of reserved word if two or more

               if P <= Text'Last and then Text (P) in 'A' .. 'Z' then
                  P := P - 1;
                  Set_Msg_Insertion_Reserved_Word (Text, P);

               --  Single upper case letter is just inserted

               else
                  Set_Msg_Char (C);
               end if;

            --  Normal character with no special treatment

            when others =>
               Set_Msg_Char (C);
         end case;
      end loop;

      VMS_Convert;
   end Set_Msg_Text;

   ----------------
   -- Set_Posted --
   ----------------

   procedure Set_Posted (N : Node_Id) is
      P : Node_Id;

   begin
      if Is_Serious_Error then

         --  We always set Error_Posted on the node itself

         Set_Error_Posted (N);

         --  If it is a subexpression, then set Error_Posted on parents up to
         --  and including the first non-subexpression construct. This helps
         --  avoid cascaded error messages within a single expression.

         P := N;
         loop
            P := Parent (P);
            exit when No (P);
            Set_Error_Posted (P);
            exit when Nkind (P) not in N_Subexpr;
         end loop;

         --  A special check, if we just posted an error on an attribute
         --  definition clause, then also set the entity involved as posted.
         --  For example, this stops complaining about the alignment after
         --  complaining about the size, which is likely to be useless.

         if Nkind (P) = N_Attribute_Definition_Clause then
            if Is_Entity_Name (Name (P)) then
               Set_Error_Posted (Entity (Name (P)));
            end if;
         end if;
      end if;
   end Set_Posted;

   -----------------------
   -- Set_Qualification --
   -----------------------

   procedure Set_Qualification (N : Nat; E : Entity_Id) is
   begin
      if N /= 0 and then Scope (E) /= Standard_Standard then
         Set_Qualification (N - 1, Scope (E));
         Set_Msg_Node (Scope (E));
         Set_Msg_Char ('.');
      end if;
   end Set_Qualification;

   ------------------------
   -- Special_Msg_Delete --
   ------------------------

   --  Is it really right to have all this specialized knowledge in errout?

   function Special_Msg_Delete
     (Msg : String;
      N   : Node_Or_Entity_Id;
      E   : Node_Or_Entity_Id) return Boolean
   is
   begin
      --  Never delete messages in -gnatdO mode

      if Debug_Flag_OO then
         return False;

      --  Processing for "atomic access cannot be guaranteed"

      elsif Msg = "atomic access to & cannot be guaranteed" then

         --  When an atomic object refers to a non-atomic type in the same
         --  scope, we implicitly make the type atomic. In the non-error case
         --  this is surely safe (and in fact prevents an error from occurring
         --  if the type is not atomic by default). But if the object cannot be
         --  made atomic, then we introduce an extra junk message by this
         --  manipulation, which we get rid of here.

         --  We identify this case by the fact that it references a type for
         --  which Is_Atomic is set, but there is no Atomic pragma setting it.

         if Is_Type (E)
           and then Is_Atomic (E)
           and then No (Get_Rep_Pragma (E, Name_Atomic))
         then
            return True;
         end if;

      --  Processing for "Size too small" messages

      elsif Msg = "size for& too small, minimum allowed is ^" then

         --  Suppress "size too small" errors in CodePeer mode and SPARK mode,
         --  since pragma Pack is also ignored in these configurations.

         if CodePeer_Mode or GNATprove_Mode then
            return True;

         --  When a size is wrong for a frozen type there is no explicit size
         --  clause, and other errors have occurred, suppress the message,
         --  since it is likely that this size error is a cascaded result of
         --  other errors. The reason we eliminate unfrozen types is that
         --  messages issued before the freeze type are for sure OK.

         elsif Is_Frozen (E)
           and then Serious_Errors_Detected > 0
           and then Nkind (N) /= N_Component_Clause
           and then Nkind (Parent (N)) /= N_Component_Clause
           and then
             No (Get_Attribute_Definition_Clause (E, Attribute_Size))
           and then
             No (Get_Attribute_Definition_Clause (E, Attribute_Object_Size))
           and then
             No (Get_Attribute_Definition_Clause (E, Attribute_Value_Size))
         then
            return True;
         end if;
      end if;

      --  All special tests complete, so go ahead with message

      return False;
   end Special_Msg_Delete;

   --------------------------
   -- Unwind_Internal_Type --
   --------------------------

   procedure Unwind_Internal_Type (Ent : in out Entity_Id) is
      Derived : Boolean := False;
      Mchar   : Character;
      Old_Ent : Entity_Id;

   begin
      --  Undo placement of a quote, since we will put it back later

      Mchar := Msg_Buffer (Msglen);

      if Mchar = '"' then
         Msglen := Msglen - 1;
      end if;

      --  The loop here deals with recursive types, we are trying to find a
      --  related entity that is not an implicit type. Note that the check with
      --  Old_Ent stops us from getting "stuck". Also, we don't output the
      --  "type derived from" message more than once in the case where we climb
      --  up multiple levels.

      Find : loop
         Old_Ent := Ent;

         --  Implicit access type, use directly designated type In Ada 2005,
         --  the designated type may be an anonymous access to subprogram, in
         --  which case we can only point to its definition.

         if Is_Access_Type (Ent) then
            if Ekind (Ent) = E_Access_Subprogram_Type
              or else Ekind (Ent) = E_Anonymous_Access_Subprogram_Type
              or else Is_Access_Protected_Subprogram_Type (Ent)
            then
               Ent := Directly_Designated_Type (Ent);

               if not Comes_From_Source (Ent) then
                  if Buffer_Ends_With ("type ") then
                     Buffer_Remove ("type ");
                  end if;

                  if Is_Itype (Ent) then
                     declare
                        Assoc : constant Node_Id :=
                          Associated_Node_For_Itype (Ent);

                     begin
                        if Nkind (Assoc) in N_Subprogram_Specification then

                           --  Anonymous access to subprogram in a signature.
                           --  Indicate the enclosing subprogram.

                           Ent :=
                             Defining_Unit_Name
                               (Associated_Node_For_Itype (Ent));
                           Set_Msg_Str
                             ("access to subprogram declared in profile of ");

                        else
                           Set_Msg_Str ("access to subprogram with profile ");
                        end if;
                     end;
                  end if;

               elsif Ekind (Ent) = E_Function then
                  Set_Msg_Str ("access to function ");
               else
                  Set_Msg_Str ("access to procedure ");
               end if;

               exit Find;

            --  Type is access to object, named or anonymous

            else
               Set_Msg_Str ("access to ");
               Ent := Directly_Designated_Type (Ent);
            end if;

         --  Classwide type

         elsif Is_Class_Wide_Type (Ent) then
            Class_Flag := True;
            Ent := Root_Type (Ent);

         --  Use base type if this is a subtype

         elsif Ent /= Base_Type (Ent) then
            Buffer_Remove ("type ");

            --  Avoid duplication "subtype of subtype of", and also replace
            --  "derived from subtype of" simply by "derived from"

            if not Buffer_Ends_With ("subtype of ")
              and then not Buffer_Ends_With ("derived from ")
            then
               Set_Msg_Str ("subtype of ");
            end if;

            Ent := Base_Type (Ent);

         --  If this is a base type with a first named subtype, use the first
         --  named subtype instead. This is not quite accurate in all cases,
         --  but it makes too much noise to be accurate and add 'Base in all
         --  cases. Note that we only do this is the first named subtype is not
         --  itself an internal name. This avoids the obvious loop (subtype ->
         --  basetype -> subtype) which would otherwise occur!)

         else
            declare
               FST : constant Entity_Id := First_Subtype (Ent);

            begin
               if not Is_Internal_Name (Chars (FST)) then
                  Ent := FST;
                  exit Find;

                  --  Otherwise use root type

               else
                  if not Derived then
                     Buffer_Remove ("type ");

                     --  Test for "subtype of type derived from" which seems
                     --  excessive and is replaced by "type derived from".

                     Buffer_Remove ("subtype of");

                     --  Avoid duplicated "type derived from type derived from"

                     if not Buffer_Ends_With ("type derived from ") then
                        Set_Msg_Str ("type derived from ");
                     end if;

                     Derived := True;
                  end if;
               end if;
            end;

            Ent := Etype (Ent);
         end if;

         --  If we are stuck in a loop, get out and settle for the internal
         --  name after all. In this case we set to kill the message if it is
         --  not the first error message (we really try hard not to show the
         --  dirty laundry of the implementation to the poor user!)

         if Ent = Old_Ent then
            Kill_Message := True;
            exit Find;
         end if;

         --  Get out if we finally found a non-internal name to use

         exit Find when not Is_Internal_Name (Chars (Ent));
      end loop Find;

      if Mchar = '"' then
         Set_Msg_Char ('"');
      end if;
   end Unwind_Internal_Type;

   -----------------
   -- VMS_Convert --
   -----------------

   procedure VMS_Convert is
      P : Natural;
      L : Natural;
      N : Natural;

   begin
      if not OpenVMS then
         return;
      end if;

      P := Msg_Buffer'First;
      loop
         if P >= Msglen then
            return;
         end if;

         if Msg_Buffer (P) = '-' then
            for G in Gnames'Range loop
               L := Gnames (G)'Length;

               --  See if we have "-ggg switch", where ggg is Gnames entry

               if P + L + 7 <= Msglen
                 and then Msg_Buffer (P + 1 .. P + L) = Gnames (G).all
                 and then Msg_Buffer (P + L + 1 .. P + L + 7) = " switch"
               then
                  --  Replace by "/vvv qualifier", where vvv is Vnames entry

                  N := Vnames (G)'Length;
                  Msg_Buffer (P + N + 11 .. Msglen + N - L + 3) :=
                    Msg_Buffer (P + L + 8 .. Msglen);
                  Msg_Buffer (P) := '/';
                  Msg_Buffer (P + 1 .. P + N) := Vnames (G).all;
                  Msg_Buffer (P + N + 1 .. P + N + 10) := " qualifier";
                  P := P + N + 10;
                  Msglen := Msglen + N - L + 3;
                  exit;
               end if;
            end loop;
         end if;

         P := P + 1;
      end loop;
   end VMS_Convert;

end Errout;
