------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               E R R O U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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
with Fname;    use Fname;
with Hostparm;
with Lib;      use Lib;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Scans;    use Scans;
with Sinput;   use Sinput;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Style;
with Uintp;    use Uintp;
with Uname;    use Uname;

package body Errout is

   Class_Flag : Boolean := False;
   --  This flag is set True when outputting a reference to a class-wide
   --  type, and is used by Add_Class to insert 'Class at the proper point

   Continuation : Boolean;
   --  Indicates if current message is a continuation. Initialized from the
   --  Msg_Cont parameter in Error_Msg_Internal and then set True if a \
   --  insertion character is encountered.

   Cur_Msg : Error_Msg_Id;
   --  Id of most recently posted error message

   Flag_Source : Source_File_Index;
   --  Source file index for source file where error is being posted

   Is_Warning_Msg : Boolean;
   --  Set by Set_Msg_Text to indicate if current message is warning message

   Is_Unconditional_Msg : Boolean;
   --  Set by Set_Msg_Text to indicate if current message is unconditional

   Kill_Message : Boolean;
   --  A flag used to kill weird messages (e.g. those containing uninterpreted
   --  implicit type references) if we have already seen at least one message
   --  already. The idea is that we hope the weird message is a junk cascaded
   --  message that should be suppressed.

   Last_Killed : Boolean := False;
   --  Set True if the most recently posted non-continuation message was
   --  killed. This is used to determine the processing of any continuation
   --  messages that follow.

   List_Pragmas_Index : Int;
   --  Index into List_Pragmas table

   List_Pragmas_Mode : Boolean;
   --  Starts True, gets set False by pragma List (Off), True by List (On)

   Manual_Quote_Mode : Boolean;
   --  Set True in manual quotation mode

   Max_Msg_Length : constant := 80 + 2 * Hostparm.Max_Line_Length;
   --  Maximum length of error message. The addition of Max_Line_Length
   --  ensures that two insertion tokens of maximum length can be accomodated.

   Msg_Buffer : String (1 .. Max_Msg_Length);
   --  Buffer used to prepare error messages

   Msglen : Integer;
   --  Number of characters currently stored in the message buffer

   Suppress_Message : Boolean;
   --  A flag used to suppress certain obviously redundant messages (i.e.
   --  those referring to a node whose type is Any_Type). This suppression
   --  is effective only if All_Errors_Mode is off.

   Suppress_Instance_Location : Boolean := False;
   --  Normally, if a # location in a message references a location within
   --  a generic template, then a note is added giving the location of the
   --  instantiation. If this variable is set True, then this note is not
   --  output. This is used for internal processing for the case of an
   --  illegal instantiation. See Error_Msg routine for further details.

   -----------------------------------
   -- Error Message Data Structures --
   -----------------------------------

   --  The error messages are stored as a linked list of error message objects
   --  sorted into ascending order by the source location (Sloc). Each object
   --  records the text of the message and its source location.

   --  The following record type and table are used to represent error
   --  messages, with one entry in the table being allocated for each message.

   type Error_Msg_Object is record
      Text : String_Ptr;
      --  Text of error message, fully expanded with all insertions

      Next : Error_Msg_Id;
      --  Pointer to next message in error chain

      Sfile : Source_File_Index;
      --  Source table index of source file. In the case of an error that
      --  refers to a template, always references the original template
      --  not an instantiation copy.

      Sptr : Source_Ptr;
      --  Flag pointer. In the case of an error that refers to a template,
      --  always references the original template, not an instantiation copy.
      --  This value is the actual place in the source that the error message
      --  will be posted.

      Fptr : Source_Ptr;
      --  Flag location used in the call to post the error. This is normally
      --  the same as Sptr, except in the case of instantiations, where it
      --  is the original flag location value. This may refer to an instance
      --  when the actual message (and hence Sptr) references the template.

      Line : Physical_Line_Number;
      --  Line number for error message

      Col : Column_Number;
      --  Column number for error message

      Warn : Boolean;
      --  True if warning message (i.e. insertion character ? appeared)

      Uncond : Boolean;
      --  True if unconditional message (i.e. insertion character ! appeared)

      Msg_Cont : Boolean;
      --  This is used for logical messages that are composed of multiple
      --  individual messages. For messages that are not part of such a
      --  group, or that are the first message in such a group. Msg_Cont
      --  is set to False. For subsequent messages in a group, Msg_Cont
      --  is set to True. This is used to make sure that such a group of
      --  messages is either suppressed or retained as a group (e.g. in
      --  the circuit that deletes identical messages).

      Deleted : Boolean;
      --  If this flag is set, the message is not printed. This is used
      --  in the circuit for deleting duplicate/redundant error messages.
   end record;

   package Errors is new Table.Table (
     Table_Component_Type => Error_Msg_Object,
     Table_Index_Type     => Error_Msg_Id,
     Table_Low_Bound      => 1,
     Table_Initial        => 200,
     Table_Increment      => 200,
     Table_Name           => "Error");

   Error_Msgs : Error_Msg_Id;
   --  The list of error messages

   --------------------------
   -- Warning Mode Control --
   --------------------------

   --  Pragma Warnings allows warnings to be turned off for a specified
   --  region of code, and the following tabl is the data structure used
   --  to keep track of these regions.

   --  It contains pairs of source locations, the first being the start
   --  location for a warnings off region, and the second being the end
   --  location. When a pragma Warnings (Off) is encountered, a new entry
   --  is established extending from the location of the pragma to the
   --  end of the current source file. A subsequent pragma Warnings (On)
   --  adjusts the end point of this entry appropriately.

   --  If all warnings are suppressed by comamnd switch, then there is a
   --  dummy entry (put there by Errout.Initialize) at the start of the
   --  table which covers all possible Source_Ptr values. Note that the
   --  source pointer values in this table always reference the original
   --  template, not an instantiation copy, in the generic case.

   type Warnings_Entry is record
      Start : Source_Ptr;
      Stop  : Source_Ptr;
   end record;

   package Warnings is new Table.Table (
     Table_Component_Type => Warnings_Entry,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 100,
     Table_Increment      => 200,
     Table_Name           => "Warnings");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Class;
   --  Add 'Class to buffer for class wide type case (Class_Flag set)

   function Buffer_Ends_With (S : String) return Boolean;
   --  Tests if message buffer ends with given string preceded by a space

   procedure Buffer_Remove (S : String);
   --  Removes given string from end of buffer if it is present
   --  at end of buffer, and preceded by a space.

   procedure Debug_Output (N : Node_Id);
   --  Called from Error_Msg_N and Error_Msg_NE to generate line of debug
   --  output giving node number (of node N) if the debug X switch is set.

   procedure Check_Duplicate_Message (M1, M2 : Error_Msg_Id);
   --  This function is passed the Id values of two error messages. If
   --  either M1 or M2 is a continuation message, or is already deleted,
   --  the call is ignored. Otherwise a check is made to see if M1 and M2
   --  are duplicated or redundant. If so, the message to be deleted and
   --  all its continuations are marked with the Deleted flag set to True.

   procedure Error_Msg_Internal
     (Msg           : String;
      Flag_Location : Source_Ptr;
      Msg_Cont      : Boolean);
   --  This is like Error_Msg, except that Flag_Location is known not to be
   --  a location within a instantiation of a generic template. The outer
   --  level routine, Error_Msg, takes care of dealing with the generic case.
   --  Msg_Cont is set True to indicate that the message is a continuation of
   --  a previous message. This means that it must have the same Flag_Location
   --  as the previous message.

   procedure Set_Next_Non_Deleted_Msg (E : in out Error_Msg_Id);
   --  Given a message id, move to next message id, but skip any deleted
   --  messages, so that this results in E on output being the first non-
   --  deleted message following the input value of E, or No_Error_Msg if
   --  the input value of E was either already No_Error_Msg, or was the
   --  last non-deleted message.

   function No_Warnings (N : Node_Or_Entity_Id) return Boolean;
   --  Determines if warnings should be suppressed for the given node

   function OK_Node (N : Node_Id) return Boolean;
   --  Determines if a node is an OK node to place an error message on (return
   --  True) or if the error message should be suppressed (return False). A
   --  message is suppressed if the node already has an error posted on it,
   --  or if it refers to an Etype that has an error posted on it, or if
   --  it references an Entity that has an error posted on it.

   procedure Output_Error_Msgs (E : in out Error_Msg_Id);
   --  Output source line, error flag, and text of stored error message and
   --  all subsequent messages for the same line and unit. On return E is
   --  set to be one higher than the last message output.

   procedure Output_Line_Number (L : Logical_Line_Number);
   --  Output a line number as six digits (with leading zeroes suppressed),
   --  followed by a period and a blank (note that this is 8 characters which
   --  means that tabs in the source line will not get messed up). Line numbers
   --  that match or are less than the last Source_Reference pragma are listed
   --  as all blanks, avoiding output of junk line numbers.

   procedure Output_Msg_Text (E : Error_Msg_Id);
   --  Outputs characters of text in the text of the error message E, excluding
   --  any final exclamation point. Note that no end of line is output, the
   --  caller is responsible for adding the end of line.

   procedure Output_Source_Line
     (L     : Physical_Line_Number;
      Sfile : Source_File_Index;
      Errs  : Boolean);
   --  Outputs text of source line L, in file S, together with preceding line
   --  number, as described above for Output_Line_Number. The Errs parameter
   --  indicates if there are errors attached to the line, which forces
   --  listing on, even in the presence of pragma List (Off).

   function Same_Error (M1, M2 : Error_Msg_Id) return Boolean;
   --  See if two messages have the same text. Returns true if the text
   --  of the two messages is identical, or if one of them is the same
   --  as the other with an appended "instance at xxx" tag.

   procedure Set_Msg_Blank;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis. Has no effect if
   --  manual quote mode is turned on.

   procedure Set_Msg_Blank_Conditional;
   --  Sets a single blank in the message if the preceding character is a
   --  non-blank character other than a left parenthesis or quote. Has no
   --  effect if manual quote mode is turned on.

   procedure Set_Msg_Char (C : Character);
   --  Add a single character to the current message. This routine does not
   --  check for special insertion characters (they are just treated as text
   --  characters if they occur).

   procedure Set_Msg_Insertion_Column;
   --  Handle column number insertion (@ insertion character)

   procedure Set_Msg_Insertion_Name;
   --  Handle name insertion (% insertion character)

   procedure Set_Msg_Insertion_Line_Number (Loc, Flag : Source_Ptr);
   --  Handle line number insertion (# insertion character). Loc is the
   --  location to be referenced, and Flag is the location at which the
   --  flag is posted (used to determine whether to add "in file xxx")

   procedure Set_Msg_Insertion_Node;
   --  Handle node (name from node) insertion (& insertion character)

   procedure Set_Msg_Insertion_Reserved_Name;
   --  Handle insertion of reserved word name (* insertion character).

   procedure Set_Msg_Insertion_Reserved_Word
     (Text : String;
      J    : in out Integer);
   --  Handle reserved word insertion (upper case letters). The Text argument
   --  is the current error message input text, and J is an index which on
   --  entry points to the first character of the reserved word, and on exit
   --  points past the last character of the reserved word.

   procedure Set_Msg_Insertion_Type_Reference (Flag : Source_Ptr);
   --  Handle type reference (right brace insertion character). Flag is the
   --  location of the flag, which is provided for the internal call to
   --  Set_Msg_Insertion_Line_Number,

   procedure Set_Msg_Insertion_Uint;
   --  Handle Uint insertion (^ insertion character)

   procedure Set_Msg_Insertion_Unit_Name;
   --  Handle unit name insertion ($ insertion character)

   procedure Set_Msg_Insertion_File_Name;
   --  Handle file name insertion (left brace insertion character)

   procedure Set_Msg_Int (Line : Int);
   --  Set the decimal representation of the argument in the error message
   --  buffer with no leading zeroes output.

   procedure Set_Msg_Name_Buffer;
   --  Output name from Name_Buffer, with surrounding quotes unless manual
   --  quotation mode is in effect.

   procedure Set_Msg_Node (Node : Node_Id);
   --  Add the sequence of characters for the name associated with the
   --  given node to the current message.

   procedure Set_Msg_Quote;
   --  Set quote if in normal quote mode, nothing if in manual quote mode

   procedure Set_Msg_Str (Text : String);
   --  Add a sequence of characters to the current message. This routine does
   --  not check for special insertion characters (they are just treated as
   --  text characters if they occur).

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr);
   --  Add a sequence of characters to the current message. The characters may
   --  be one of the special insertion characters (see documentation in spec).
   --  Flag is the location at which the error is to be posted, which is used
   --  to determine whether or not the # insertion needs a file name. The
   --  variables Msg_Buffer, Msglen, Is_Warning_Msg, and Is_Unconditional_Msg
   --  are set on return.

   procedure Set_Posted (N : Node_Id);
   --  Sets the Error_Posted flag on the given node, and all its parents
   --  that are subexpressions and then on the parent non-subexpression
   --  construct that contains the original expression (this reduces the
   --  number of cascaded messages)

   procedure Set_Qualification (N : Nat; E : Entity_Id);
   --  Outputs up to N levels of qualification for the given entity. For
   --  example, the entity A.B.C.D will output B.C. if N = 2.

   procedure Test_Warning_Msg (Msg : String);
   --  Sets Is_Warning_Msg true if Msg is a warning message (contains a
   --  question mark character), and False otherwise.

   procedure Unwind_Internal_Type (Ent : in out Entity_Id);
   --  This procedure is given an entity id for an internal type, i.e.
   --  a type with an internal name. It unwinds the type to try to get
   --  to something reasonably printable, generating prefixes like
   --  "subtype of", "access to", etc along the way in the buffer. The
   --  value in Ent on return is the final name to be printed. Hopefully
   --  this is not an internal name, but in some internal name cases, it
   --  is an internal name, and has to be printed anyway (although in this
   --  case the message has been killed if possible). The global variable
   --  Class_Flag is set to True if the resulting entity should have
   --  'Class appended to its name (see Add_Class procedure), and is
   --  otherwise unchanged.

   function Warnings_Suppressed (Loc : Source_Ptr) return Boolean;
   --  Determines if given location is covered by a warnings off suppression
   --  range in the warnings table (or is suppressed by compilation option,
   --  which generates a warning range for the whole source file).

   ---------------
   -- Add_Class --
   ---------------

   procedure Add_Class is
   begin
      if Class_Flag then
         Class_Flag := False;
         Set_Msg_Char (''');
         Get_Name_String (Name_Class);
         Set_Casing (Identifier_Casing (Flag_Source), Mixed_Case);
         Set_Msg_Name_Buffer;
      end if;
   end Add_Class;

   ----------------------
   -- Buffer_Ends_With --
   ----------------------

   function Buffer_Ends_With (S : String) return Boolean is
      Len : constant Natural := S'Length;

   begin
      return
        Msglen > Len
          and then Msg_Buffer (Msglen - Len) = ' '
          and then Msg_Buffer (Msglen - Len + 1 .. Msglen) = S;
   end Buffer_Ends_With;

   -------------------
   -- Buffer_Remove --
   -------------------

   procedure Buffer_Remove (S : String) is
   begin
      if Buffer_Ends_With (S) then
         Msglen := Msglen - S'Length;
      end if;
   end Buffer_Remove;

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

   -----------------------------
   -- Check_Duplicate_Message --
   -----------------------------

   procedure Check_Duplicate_Message (M1, M2 : Error_Msg_Id) is
      L1, L2 : Error_Msg_Id;
      N1, N2 : Error_Msg_Id;

      procedure Delete_Msg (Delete, Keep : Error_Msg_Id);
      --  Called to delete message Delete, keeping message Keep. Marks
      --  all messages of Delete with deleted flag set to True, and also
      --  makes sure that for the error messages that are retained the
      --  preferred message is the one retained (we prefer the shorter
      --  one in the case where one has an Instance tag). Note that we
      --  always know that Keep has at least as many continuations as
      --  Delete (since we always delete the shorter sequence).

      procedure Delete_Msg (Delete, Keep : Error_Msg_Id) is
         D, K : Error_Msg_Id;

      begin
         D := Delete;
         K := Keep;

         loop
            Errors.Table (D).Deleted := True;

            --  Adjust error message count

            if Errors.Table (D).Warn then
               Warnings_Detected := Warnings_Detected - 1;
            else
               Errors_Detected := Errors_Detected - 1;
            end if;

            --  Substitute shorter of the two error messages

            if Errors.Table (K).Text'Length > Errors.Table (D).Text'Length then
               Errors.Table (K).Text := Errors.Table (D).Text;
            end if;

            D := Errors.Table (D).Next;
            K := Errors.Table (K).Next;

            if D = No_Error_Msg or else not Errors.Table (D).Msg_Cont then
               return;
            end if;
         end loop;
      end Delete_Msg;

   --  Start of processing for Check_Duplicate_Message

   begin
      --  Both messages must be non-continuation messages and not deleted

      if Errors.Table (M1).Msg_Cont
        or else Errors.Table (M2).Msg_Cont
        or else Errors.Table (M1).Deleted
        or else Errors.Table (M2).Deleted
      then
         return;
      end if;

      --  Definitely not equal if message text does not match

      if not Same_Error (M1, M2) then
         return;
      end if;

      --  Same text. See if all continuations are also identical

      L1 := M1;
      L2 := M2;

      loop
         N1 := Errors.Table (L1).Next;
         N2 := Errors.Table (L2).Next;

         --  If M1 continuations have run out, we delete M1, either the
         --  messages have the same number of continuations, or M2 has
         --  more and we prefer the one with more anyway.

         if N1 = No_Error_Msg or else not Errors.Table (N1).Msg_Cont then
            Delete_Msg (M1, M2);
            return;

         --  If M2 continuatins have run out, we delete M2

         elsif N2 = No_Error_Msg or else not Errors.Table (N2).Msg_Cont then
            Delete_Msg (M2, M1);
            return;

         --  Otherwise see if continuations are the same, if not, keep both
         --  sequences, a curious case, but better to keep everything!

         elsif not Same_Error (N1, N2) then
            return;

         --  If continuations are the same, continue scan

         else
            L1 := N1;
            L2 := N2;
         end if;
      end loop;
   end Check_Duplicate_Message;

   ------------------------
   -- Compilation_Errors --
   ------------------------

   function Compilation_Errors return Boolean is
   begin
      return Errors_Detected /= 0
        or else (Warnings_Detected /= 0
                  and then Warning_Mode = Treat_As_Error);
   end Compilation_Errors;

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (N : Node_Id) is
   begin
      if Debug_Flag_1 then
         Write_Str ("*** following error message posted on node id = #");
         Write_Int (Int (N));
         Write_Str (" ***");
         Write_Eol;
      end if;
   end Debug_Output;

   ----------
   -- dmsg --
   ----------

   procedure dmsg (Id : Error_Msg_Id) is
      E : Error_Msg_Object renames Errors.Table (Id);

   begin
      w ("Dumping error message, Id = ", Int (Id));
      w ("  Text     = ", E.Text.all);
      w ("  Next     = ", Int (E.Next));
      w ("  Sfile    = ", Int (E.Sfile));

      Write_Str
        ("  Sptr     = ");
      Write_Location (E.Sptr);
      Write_Eol;

      Write_Str
        ("  Fptr     = ");
      Write_Location (E.Fptr);
      Write_Eol;

      w ("  Line     = ", Int (E.Line));
      w ("  Col      = ", Int (E.Col));
      w ("  Warn     = ", E.Warn);
      w ("  Uncond   = ", E.Uncond);
      w ("  Msg_Cont = ", E.Msg_Cont);
      w ("  Deleted  = ", E.Deleted);

      Write_Eol;
   end dmsg;

   ---------------
   -- Error_Msg --
   ---------------

   --  Error_Msg posts a flag at the given location, except that if the
   --  Flag_Location points within a generic template and corresponds
   --  to an instantiation of this generic template, then the actual
   --  message will be posted on the generic instantiation, along with
   --  additional messages referencing the generic declaration.

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr) is
      Sindex : Source_File_Index;
      --  Source index for flag location

      Orig_Loc : Source_Ptr;
      --  Original location of Flag_Location (i.e. location in original
      --  template in instantiation case, otherwise unchanged).

   begin
      --  If we already have messages, and we are trying to place a message
      --  at No_Location or in package Standard, then just ignore the attempt
      --  since we assume that what is happening is some cascaded junk. Note
      --  that this is safe in the sense that proceeding will surely bomb.

      if Flag_Location < First_Source_Ptr
        and then Errors_Detected > 0
      then
         return;
      end if;

      Sindex := Get_Source_File_Index (Flag_Location);
      Test_Warning_Msg (Msg);

      --  It is a fatal error to issue an error message when scanning from
      --  the internal source buffer (see Sinput for further documentation)

      pragma Assert (Source /= Internal_Source_Ptr);

      --  Ignore warning message that is suppressed

      Orig_Loc := Original_Location (Flag_Location);

      if Is_Warning_Msg and then Warnings_Suppressed (Orig_Loc) then
         return;
      end if;

      --  The idea at this stage is that we have two kinds of messages.

      --  First, we have those that are to be placed as requested at
      --  Flag_Location. This includes messages that have nothing to
      --  do with generics, and also messages placed on generic templates
      --  that reflect an error in the template itself. For such messages
      --  we simply call Error_Msg_Internal to place the message in the
      --  requested location.

      if Instantiation (Sindex) = No_Location then
         Error_Msg_Internal (Msg, Flag_Location, False);
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

      --  However, before we do this, we need to worry about the case where
      --  indeed we are in an instantiation, but the message is a warning
      --  message. In this case, it almost certainly a warning for the
      --  template itself and so it is posted on the template. At least
      --  this is the default mode, it can be cancelled (resulting the
      --  warning being placed on the instance as in the error case) by
      --  setting the global Warn_On_Instance True.

      if (not Warn_On_Instance) and then Is_Warning_Msg then
         Error_Msg_Internal (Msg, Flag_Location, False);
         return;
      end if;

      --  Second, we need to worry about the case where there was a real error
      --  in the template, and we are getting a repeat of this error in the
      --  instantiation. We don't want to complain about the instantiation
      --  in this case, since we have already flagged the template.

      --  To deal with this case, just see if we have posted a message at
      --  the template location already. If so, assume that the current
      --  message is redundant. There could be cases in which this is not
      --  a correct assumption, but it is not terrible to lose a message
      --  about an incorrect instantiation given that we have already
      --  flagged a message on the template.

      for Err in Errors.First .. Errors.Last loop
         if Errors.Table (Err).Sptr = Orig_Loc then

         --  If the current message is a real error, as opposed to a
         --  warning, then we don't want to let a warning on the
         --  template inhibit a real error on the instantiation.

            if Is_Warning_Msg
              or else not Errors.Table (Err).Warn
            then
               return;
            end if;
         end if;
      end loop;

      --  OK, this is the case where we have an instantiation error, and
      --  we need to generate the error on the instantiation, rather than
      --  on the template. First, see if we have posted this exact error
      --  before, and if so suppress it. It is not so easy to use the main
      --  list of errors for this, since they have already been split up
      --  according to the processing below. Consequently we use an auxiliary
      --  data structure that just records these types of messages (it will
      --  never have very many entries).

      declare
         Actual_Error_Loc : Source_Ptr;
         --  Location of outer level instantiation in instantiation case, or
         --  just a copy of Flag_Location in the normal case. This is the
         --  location where all error messages will actually be posted.

         Save_Error_Msg_Sloc : constant Source_Ptr := Error_Msg_Sloc;
         --  Save possible location set for caller's message. We need to
         --  use Error_Msg_Sloc for the location of the instantiation error
         --  but we have to preserve a possible original value.

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

         --  Since we are generating the messages at the instantiation
         --  point in any case, we do not want the references to the
         --  bad lines in the instance to be annotated with the location
         --  of the instantiation.

         Suppress_Instance_Location := True;
         Msg_Cont_Status := False;

         --  Loop to generate instantiation messages

         Error_Msg_Sloc := Flag_Location;
         X := Get_Source_File_Index (Flag_Location);

         while Instantiation (X) /= No_Location loop

            --  Suppress instantiation message on continuation lines

            if Msg (1) /= '\' then
               if Is_Warning_Msg then
                  Error_Msg_Internal
                    ("?in instantiation #",
                     Actual_Error_Loc, Msg_Cont_Status);

               else
                  Error_Msg_Internal
                    ("instantiation error #",
                     Actual_Error_Loc, Msg_Cont_Status);
               end if;
            end if;

            Error_Msg_Sloc := Instantiation (X);
            X := Get_Source_File_Index (Error_Msg_Sloc);
            Msg_Cont_Status := True;
         end loop;

         Suppress_Instance_Location := False;
         Error_Msg_Sloc := Save_Error_Msg_Sloc;

         --  Here we output the original message on the outer instantiation

         Error_Msg_Internal (Msg, Actual_Error_Loc, Msg_Cont_Status);
      end;
   end Error_Msg;

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
      --  into the following token (i.e. the current token)

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

   ------------------------
   -- Error_Msg_Internal --
   ------------------------

   procedure Error_Msg_Internal
     (Msg           : String;
      Flag_Location : Source_Ptr;
      Msg_Cont      : Boolean)
   is
      Next_Msg : Error_Msg_Id;
      --  Pointer to next message at insertion point

      Prev_Msg : Error_Msg_Id;
      --  Pointer to previous message at insertion point

      Temp_Msg : Error_Msg_Id;

      Orig_Loc : constant Source_Ptr := Original_Location (Flag_Location);

      procedure Handle_Fatal_Error;
      --  Internal procedure to do all error message handling other than
      --  bumping the error count and arranging for the message to be output.

      procedure Handle_Fatal_Error is
      begin
         --  Turn off code generation if not done already

         if Operating_Mode = Generate_Code then
            Operating_Mode := Check_Semantics;
            Expander_Active := False;
         end if;

         --  Set the fatal error flag in the unit table unless we are
         --  in Try_Semantics mode. This stops the semantics from being
         --  performed if we find a parser error. This is skipped if we
         --  are currently dealing with the configuration pragma file.

         if not Try_Semantics
           and then Current_Source_Unit /= No_Unit
         then
            Set_Fatal_Error (Get_Source_Unit (Orig_Loc));
         end if;
      end Handle_Fatal_Error;

   --  Start of processing for Error_Msg_Internal

   begin
      if Raise_Exception_On_Error /= 0 then
         raise Error_Msg_Exception;
      end if;

      Continuation := Msg_Cont;
      Suppress_Message := False;
      Kill_Message := False;
      Set_Msg_Text (Msg, Orig_Loc);

      --  Kill continuation if parent message killed

      if Continuation and Last_Killed then
         return;
      end if;

      --  Return without doing anything if message is suppressed

      if Suppress_Message
        and not All_Errors_Mode
        and not (Msg (Msg'Last) = '!')
      then
         if not Continuation then
            Last_Killed := True;
         end if;

         return;
      end if;

      --  Return without doing anything if message is killed and this
      --  is not the first error message. The philosophy is that if we
      --  get a weird error message and we already have had a message,
      --  then we hope the weird message is a junk cascaded message

      if Kill_Message
        and then not All_Errors_Mode
        and then Errors_Detected /= 0
      then
         if not Continuation then
            Last_Killed := True;
         end if;

         return;
      end if;

      --  Immediate return if warning message and warnings are suppressed

      if Is_Warning_Msg and then Warnings_Suppressed (Orig_Loc) then
         Cur_Msg := No_Error_Msg;
         return;
      end if;

      --  If message is to be ignored in special ignore message mode, this is
      --  where we do this special processing, bypassing message output.

      if Ignore_Errors_Enable > 0 then
         Handle_Fatal_Error;
         return;
      end if;

      --  Otherwise build error message object for new message

      Errors.Increment_Last;
      Cur_Msg := Errors.Last;
      Errors.Table (Cur_Msg).Text     := new String'(Msg_Buffer (1 .. Msglen));
      Errors.Table (Cur_Msg).Next     := No_Error_Msg;
      Errors.Table (Cur_Msg).Sptr     := Orig_Loc;
      Errors.Table (Cur_Msg).Fptr     := Flag_Location;
      Errors.Table (Cur_Msg).Sfile    := Get_Source_File_Index (Orig_Loc);
      Errors.Table (Cur_Msg).Line     := Get_Physical_Line_Number (Orig_Loc);
      Errors.Table (Cur_Msg).Col      := Get_Column_Number (Orig_Loc);
      Errors.Table (Cur_Msg).Warn     := Is_Warning_Msg;
      Errors.Table (Cur_Msg).Uncond   := Is_Unconditional_Msg;
      Errors.Table (Cur_Msg).Msg_Cont := Continuation;
      Errors.Table (Cur_Msg).Deleted  := False;

      --  If immediate errors mode set, output error message now. Also output
      --  now if the -d1 debug flag is set (so node number message comes out
      --  just before actual error message)

      if Debug_Flag_OO or else Debug_Flag_1 then
         Write_Eol;
         Output_Source_Line (Errors.Table (Cur_Msg).Line,
           Errors.Table (Cur_Msg).Sfile, True);
         Temp_Msg := Cur_Msg;
         Output_Error_Msgs (Temp_Msg);

      --  If not in immediate errors mode, then we insert the message in the
      --  error chain for later output by Finalize. The messages are sorted
      --  first by unit (main unit comes first), and within a unit by source
      --  location (earlier flag location first in the chain).

      else
         Prev_Msg := No_Error_Msg;
         Next_Msg := Error_Msgs;

         while Next_Msg /= No_Error_Msg loop
            exit when
              Errors.Table (Cur_Msg).Sfile < Errors.Table (Next_Msg).Sfile;

            if Errors.Table (Cur_Msg).Sfile =
                 Errors.Table (Next_Msg).Sfile
            then
               exit when Orig_Loc < Errors.Table (Next_Msg).Sptr;
            end if;

            Prev_Msg := Next_Msg;
            Next_Msg := Errors.Table (Next_Msg).Next;
         end loop;

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
            --  Don't delete unconditional messages and at this stage,
            --  don't delete continuation lines (we attempted to delete
            --  those earlier if the parent message was deleted.

            if not Errors.Table (Cur_Msg).Uncond
              and then not Continuation
            then

               --  Don't delete if prev msg is warning and new msg is
               --  an error. This is because we don't want a real error
               --  masked by a warning. In all other cases (that is parse
               --  errors for the same line that are not unconditional)
               --  we do delete the message. This helps to avoid
               --  junk extra messages from cascaded parsing errors

               if not Errors.Table (Prev_Msg).Warn
                 or else Errors.Table (Cur_Msg).Warn
               then
                  --  All tests passed, delete the message by simply
                  --  returning without any further processing.

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
            Error_Msgs := Cur_Msg;
         else
            Errors.Table (Prev_Msg).Next := Cur_Msg;
         end if;

         Errors.Table (Cur_Msg).Next := Next_Msg;
      end if;

      --  Bump appropriate statistics count

      if Errors.Table (Cur_Msg).Warn then
         Warnings_Detected := Warnings_Detected + 1;
      else
         Errors_Detected := Errors_Detected + 1;
         Handle_Fatal_Error;
      end if;

      --  Terminate if max errors reached

      if Errors_Detected + Warnings_Detected = Maximum_Errors then
         raise Unrecoverable_Error;
      end if;

   end Error_Msg_Internal;

   -----------------
   -- Error_Msg_N --
   -----------------

   procedure Error_Msg_N (Msg : String; N : Node_Or_Entity_Id) is
   begin
      if No_Warnings (N) then
         Test_Warning_Msg (Msg);

         if Is_Warning_Msg then
            return;
         end if;
      end if;

      if All_Errors_Mode
        or else Msg (Msg'Last) = '!'
        or else OK_Node (N)
        or else (Msg (1) = '\' and not Last_Killed)
      then
         Debug_Output (N);
         Error_Msg_Node_1 := N;
         Error_Msg (Msg, Sloc (N));

      else
         Last_Killed := True;
      end if;

      if not Is_Warning_Msg then
         Set_Posted (N);
      end if;
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
      if No_Warnings (N) or else No_Warnings (E) then
         Test_Warning_Msg (Msg);

         if Is_Warning_Msg then
            return;
         end if;
      end if;

      if All_Errors_Mode
        or else Msg (Msg'Last) = '!'
        or else OK_Node (N)
        or else (Msg (1) = '\' and not Last_Killed)
      then
         Debug_Output (N);
         Error_Msg_Node_1 := E;
         Error_Msg (Msg, Sloc (N));

      else
         Last_Killed := True;
      end if;

      if not Is_Warning_Msg then
         Set_Posted (N);
      end if;
   end Error_Msg_NE;

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

   procedure Finalize is
      Cur      : Error_Msg_Id;
      Nxt      : Error_Msg_Id;
      E, F     : Error_Msg_Id;
      Err_Flag : Boolean;

   begin
      --  Reset current error source file if the main unit has a pragma
      --  Source_Reference. This ensures outputting the proper name of
      --  the source file in this situation.

      if Num_SRef_Pragmas (Main_Source_File) /= 0 then
         Current_Error_Source_File := No_Source_File;
      end if;

      --  Eliminate any duplicated error messages from the list. This is
      --  done after the fact to avoid problems with Change_Error_Text.

      Cur := Error_Msgs;
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

      --  Brief Error mode

      if Brief_Output or (not Full_List and not Verbose_Mode) then
         E := Error_Msgs;
         Set_Standard_Error;

         while E /= No_Error_Msg loop
            if not Errors.Table (E).Deleted and then not Debug_Flag_KK then
               Write_Name (Reference_Name (Errors.Table (E).Sfile));
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
         E := Error_Msgs;
         Write_Eol;

         --  First list initial main source file with its error messages

         for N in 1 .. Last_Source_Line (Main_Source_File) loop
            Err_Flag :=
              E /= No_Error_Msg
                and then Errors.Table (E).Line = N
                and then Errors.Table (E).Sfile = Main_Source_File;

            Output_Source_Line (N, Main_Source_File, Err_Flag);

            if Err_Flag then
               Output_Error_Msgs (E);

               if not Debug_Flag_2 then
                  Write_Eol;
               end if;
            end if;

         end loop;

         --  Then output errors, if any, for subsidiary units

         while E /= No_Error_Msg
           and then Errors.Table (E).Sfile /= Main_Source_File
         loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line, Errors.Table (E).Sfile, True);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Verbose mode (error lines only with error flags)

      if Verbose_Mode and not Full_List then
         E := Error_Msgs;

         --  Loop through error lines

         while E /= No_Error_Msg loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line, Errors.Table (E).Sfile, True);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Output error summary if verbose or full list mode

      if Verbose_Mode or else Full_List then

         --  Extra blank line if error messages or source listing were output

         if Errors_Detected + Warnings_Detected > 0 or else Full_List then
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

         if Errors_Detected + Warnings_Detected /= 0
           and then not Brief_Output
           and then (Verbose_Mode or Full_List)
         then
            Set_Standard_Error;
         end if;

         --  Message giving total number of lines

         Write_Str (" ");
         Write_Int (Num_Source_Lines (Main_Source_File));

         if Num_Source_Lines (Main_Source_File) = 1 then
            Write_Str (" line: ");
         else
            Write_Str (" lines: ");
         end if;

         if Errors_Detected = 0 then
            Write_Str ("No errors");

         elsif Errors_Detected = 1 then
            Write_Str ("1 error");

         else
            Write_Int (Errors_Detected);
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
      end if;

      if Maximum_Errors /= 0
        and then Errors_Detected + Warnings_Detected = Maximum_Errors
      then
         Set_Standard_Error;
         Write_Str ("fatal error: maximum errors reached");
         Write_Eol;
         Set_Standard_Output;
      end if;

      if Warning_Mode = Treat_As_Error then
         Errors_Detected := Errors_Detected + Warnings_Detected;
         Warnings_Detected := 0;
      end if;

   end Finalize;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (E : Error_Msg_Id) return Source_Ptr is
   begin
      return Errors.Table (E).Sptr;
   end Get_Location;

   ----------------
   -- Get_Msg_Id --
   ----------------

   function Get_Msg_Id return Error_Msg_Id is
   begin
      return Cur_Msg;
   end Get_Msg_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Errors.Init;
      Error_Msgs := No_Error_Msg;
      Errors_Detected := 0;
      Warnings_Detected := 0;
      Cur_Msg := No_Error_Msg;
      List_Pragmas.Init;

      --  Initialize warnings table, if all warnings are suppressed, supply
      --  an initial dummy entry covering all possible source locations.

      Warnings.Init;

      if Warning_Mode = Suppress then
         Warnings.Increment_Last;
         Warnings.Table (Warnings.Last).Start := Source_Ptr'First;
         Warnings.Table (Warnings.Last).Stop  := Source_Ptr'Last;
      end if;

   end Initialize;

   -----------------
   -- No_Warnings --
   -----------------

   function No_Warnings (N : Node_Or_Entity_Id) return Boolean is
   begin
      if Error_Posted (N) then
         return True;

      elsif Nkind (N) in N_Entity and then Warnings_Off (N) then
         return True;

      elsif Is_Entity_Name (N)
        and then Present (Entity (N))
        and then Warnings_Off (Entity (N))
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

   -----------------------
   -- Output_Error_Msgs --
   -----------------------

   procedure Output_Error_Msgs (E : in out Error_Msg_Id) is
      P : Source_Ptr;
      T : Error_Msg_Id;
      S : Error_Msg_Id;

      Flag_Num   : Pos;
      Mult_Flags : Boolean := False;

   begin
      S := E;

      --  Skip deleted messages at start

      if Errors.Table (S).Deleted then
         Set_Next_Non_Deleted_Msg (S);
      end if;

      --  Figure out if we will place more than one error flag on this line

      T := S;
      while T /= No_Error_Msg
        and then Errors.Table (T).Line = Errors.Table (E).Line
        and then Errors.Table (T).Sfile = Errors.Table (E).Sfile
      loop
         if Errors.Table (T).Sptr > Errors.Table (E).Sptr then
            Mult_Flags := True;
         end if;

         Set_Next_Non_Deleted_Msg (T);
      end loop;

      --  Output the error flags. The circuit here makes sure that the tab
      --  characters in the original line are properly accounted for. The
      --  eight blanks at the start are to match the line number.

      if not Debug_Flag_2 then
         Write_Str ("        ");
         P := Line_Start (Errors.Table (E).Sptr);
         Flag_Num := 1;

         --  Loop through error messages for this line to place flags

         T := S;
         while T /= No_Error_Msg
           and then Errors.Table (T).Line = Errors.Table (E).Line
           and then Errors.Table (T).Sfile = Errors.Table (E).Sfile
         loop
            --  Loop to output blanks till current flag position

            while P < Errors.Table (T).Sptr loop
               if Source_Text (Errors.Table (T).Sfile) (P) = ASCII.HT then
                  Write_Char (ASCII.HT);
               else
                  Write_Char (' ');
               end if;

               P := P + 1;
            end loop;

            --  Output flag (unless already output, this happens if more
            --  than one error message occurs at the same flag position).

            if P = Errors.Table (T).Sptr then
               if (Flag_Num = 1 and then not Mult_Flags)
                 or else Flag_Num > 9
               then
                  Write_Char ('|');
               else
                  Write_Char (Character'Val (Character'Pos ('0') + Flag_Num));
               end if;

               P := P + 1;
            end if;

            Set_Next_Non_Deleted_Msg (T);
            Flag_Num := Flag_Num + 1;
         end loop;

         Write_Eol;
      end if;

      --  Now output the error messages

      T := S;
      while T /= No_Error_Msg
        and then Errors.Table (T).Line = Errors.Table (E).Line
        and then Errors.Table (T).Sfile = Errors.Table (E).Sfile

      loop
         Write_Str ("        >>> ");
         Output_Msg_Text (T);

         if Debug_Flag_2 then
            while Column < 74 loop
               Write_Char (' ');
            end loop;

            Write_Str (" <<<");
         end if;

         Write_Eol;
         Set_Next_Non_Deleted_Msg (T);
      end loop;

      E := T;
   end Output_Error_Msgs;

   ------------------------
   -- Output_Line_Number --
   ------------------------

   procedure Output_Line_Number (L : Logical_Line_Number) is
      D     : Int;       -- next digit
      C     : Character; -- next character
      Z     : Boolean;   -- flag for zero suppress
      N, M  : Int;       -- temporaries

   begin
      if L = No_Line_Number then
         Write_Str ("        ");

      else
         Z := False;
         N := Int (L);

         M := 100_000;
         while M /= 0 loop
            D := Int (N / M);
            N := N rem M;
            M := M / 10;

            if D = 0 then
               if Z then
                  C := '0';
               else
                  C := ' ';
               end if;
            else
               Z := True;
               C := Character'Val (D + 48);
            end if;

            Write_Char (C);
         end loop;

         Write_Str (". ");
      end if;
   end Output_Line_Number;

   ---------------------
   -- Output_Msg_Text --
   ---------------------

   procedure Output_Msg_Text (E : Error_Msg_Id) is
   begin
      if Errors.Table (E).Warn then
         if Errors.Table (E).Text'Length > 7
           and then Errors.Table (E).Text (1 .. 7) /= "(style)"
         then
            Write_Str ("warning: ");
         end if;

      elsif Opt.Unique_Error_Tag then
         Write_Str ("error: ");
      end if;

      Write_Str (Errors.Table (E).Text.all);
   end Output_Msg_Text;

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

   begin
      if Sfile /= Current_Error_Source_File then
         Write_Str ("==============Error messages for source file: ");
         Write_Name (Full_File_Name (Sfile));
         Write_Eol;

         if Num_SRef_Pragmas (Sfile) > 0 then
            Write_Str ("--------------Line numbers from file: ");
            Write_Name (Full_Ref_Name (Sfile));

            --  Write starting line, except do not write it if we had more
            --  than one source reference pragma, since in this case there
            --  is no very useful number to write.

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

         S := S + 1;
      end loop;

      if Line_Number_Output then
         Write_Eol;
      end if;
   end Output_Source_Line;

   --------------------
   -- Purge_Messages --
   --------------------

   procedure Purge_Messages (From : Source_Ptr; To : Source_Ptr) is
      E : Error_Msg_Id;

      function To_Be_Purged (E : Error_Msg_Id) return Boolean;
      --  Returns True for a message that is to be purged. Also adjusts
      --  error counts appropriately.

      function To_Be_Purged (E : Error_Msg_Id) return Boolean is
      begin
         if E /= No_Error_Msg
           and then Errors.Table (E).Sptr > From
           and then Errors.Table (E).Sptr < To
         then
            if Errors.Table (E).Warn then
               Warnings_Detected := Warnings_Detected - 1;
            else
               Errors_Detected := Errors_Detected - 1;
            end if;

            return True;

         else
            return False;
         end if;
      end To_Be_Purged;

   --  Start of processing for Purge_Messages

   begin
      while To_Be_Purged (Error_Msgs) loop
         Error_Msgs := Errors.Table (Error_Msgs).Next;
      end loop;

      E := Error_Msgs;
      while E /= No_Error_Msg loop
         while To_Be_Purged (Errors.Table (E).Next) loop
            Errors.Table (E).Next :=
              Errors.Table (Errors.Table (E).Next).Next;
         end loop;

         E := Errors.Table (E).Next;
      end loop;
   end Purge_Messages;

   -----------------------------
   -- Remove_Warning_Messages --
   -----------------------------

   procedure Remove_Warning_Messages (N : Node_Id) is

      function Check_For_Warning (N : Node_Id) return Traverse_Result;
      --  This function checks one node for a possible warning message.

      function Check_All_Warnings is new
        Traverse_Func (Check_For_Warning);
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
              and then Errors.Table (E).Fptr = Loc
              and then Errors.Table (E).Warn
            then
               Warnings_Detected := Warnings_Detected - 1;
               return True;
            else
               return False;
            end if;
         end To_Be_Removed;

      --  Start of processing for Check_For_Warnings

      begin
         while To_Be_Removed (Error_Msgs) loop
            Error_Msgs := Errors.Table (Error_Msgs).Next;
         end loop;

         E := Error_Msgs;
         while E /= No_Error_Msg loop
            while To_Be_Removed (Errors.Table (E).Next) loop
               Errors.Table (E).Next :=
                 Errors.Table (Errors.Table (E).Next).Next;
            end loop;

            E := Errors.Table (E).Next;
         end loop;

         if Nkind (N) = N_Raise_Constraint_Error
           and then Original_Node (N) /= N
         then
            --  Warnings may have been posted on subexpressions of
            --  the original tree. We temporarily replace the raise
            --  statement with the original expression to remove
            --  those warnings, whose sloc do not match those of
            --  any node in the current tree.

            declare
               Old    : Node_Id := N;
               Status : Traverse_Result;

            begin
               Rewrite (N, Original_Node (N));
               Status := Check_For_Warning (N);
               Rewrite (N, Old);
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
            Discard : Traverse_Result;
         begin
            Discard := Check_All_Warnings (N);
         end;
      end if;
   end Remove_Warning_Messages;

   ----------------
   -- Same_Error --
   ----------------

   function Same_Error (M1, M2 : Error_Msg_Id) return Boolean is
      Msg1 : constant String_Ptr := Errors.Table (M1).Text;
      Msg2 : constant String_Ptr := Errors.Table (M2).Text;

      Msg2_Len : constant Integer := Msg2'Length;
      Msg1_Len : constant Integer := Msg1'Length;

   begin
      return
        Msg1.all = Msg2.all
          or else
            (Msg1_Len - 10 > Msg2_Len
               and then
             Msg2.all = Msg1.all (1 .. Msg2_Len)
               and then
             Msg1 (Msg2_Len + 1 .. Msg2_Len + 10) = ", instance")
          or else
            (Msg2_Len - 10 > Msg1_Len
               and then
             Msg1.all = Msg2.all (1 .. Msg1_Len)
               and then
             Msg2 (Msg1_Len + 1 .. Msg1_Len + 10) = ", instance");
   end Same_Error;

   -------------------
   -- Set_Msg_Blank --
   -------------------

   procedure Set_Msg_Blank is
   begin
      if Msglen > 0
        and then Msg_Buffer (Msglen) /= ' '
        and then Msg_Buffer (Msglen) /= '('
        and then not Manual_Quote_Mode
      then
         Set_Msg_Char (' ');
      end if;
   end Set_Msg_Blank;

   -------------------------------
   -- Set_Msg_Blank_Conditional --
   -------------------------------

   procedure Set_Msg_Blank_Conditional is
   begin
      if Msglen > 0
        and then Msg_Buffer (Msglen) /= ' '
        and then Msg_Buffer (Msglen) /= '('
        and then Msg_Buffer (Msglen) /= '"'
        and then not Manual_Quote_Mode
      then
         Set_Msg_Char (' ');
      end if;
   end Set_Msg_Blank_Conditional;

   ------------------
   -- Set_Msg_Char --
   ------------------

   procedure Set_Msg_Char (C : Character) is
   begin

      --  The check for message buffer overflow is needed to deal with cases
      --  where insertions get too long (in particular a child unit name can
      --  be very long).

      if Msglen < Max_Msg_Length then
         Msglen := Msglen + 1;
         Msg_Buffer (Msglen) := C;
      end if;
   end Set_Msg_Char;

   ------------------------------
   -- Set_Msg_Insertion_Column --
   ------------------------------

   procedure Set_Msg_Insertion_Column is
   begin
      if Style.RM_Column_Check then
         Set_Msg_Str (" in column ");
         Set_Msg_Int (Int (Error_Msg_Col) + 1);
      end if;
   end Set_Msg_Insertion_Column;

   ---------------------------------
   -- Set_Msg_Insertion_File_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_File_Name is
   begin
      if Error_Msg_Name_1 = No_Name then
         null;

      elsif Error_Msg_Name_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank;
         Get_Name_String (Error_Msg_Name_1);
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignments ensure that the second and third percent
      --  insertion characters will correspond to the Error_Msg_Name_2 and
      --  Error_Msg_Name_3 as required.

      Error_Msg_Name_1 := Error_Msg_Name_2;
      Error_Msg_Name_2 := Error_Msg_Name_3;

   end Set_Msg_Insertion_File_Name;

   -----------------------------------
   -- Set_Msg_Insertion_Line_Number --
   -----------------------------------

   procedure Set_Msg_Insertion_Line_Number (Loc, Flag : Source_Ptr) is
      Sindex_Loc  : Source_File_Index;
      Sindex_Flag : Source_File_Index;

   begin
      Set_Msg_Blank;

      if Loc = No_Location then
         Set_Msg_Str ("at unknown location");

      elsif Loc <= Standard_Location then
         Set_Msg_Str ("in package Standard");

         if Loc = Standard_ASCII_Location then
            Set_Msg_Str (".ASCII");
         end if;

      else
         --  Add "at file-name:" if reference is to other than the source
         --  file in which the error message is placed. Note that we check
         --  full file names, rather than just the source indexes, to
         --  deal with generic instantiations from the current file.

         Sindex_Loc  := Get_Source_File_Index (Loc);
         Sindex_Flag := Get_Source_File_Index (Flag);

         if Full_File_Name (Sindex_Loc) /= Full_File_Name (Sindex_Flag) then
            Set_Msg_Str ("at ");
            Get_Name_String
              (Reference_Name (Get_Source_File_Index (Loc)));
            Set_Msg_Name_Buffer;
            Set_Msg_Char (':');

         --  If in current file, add text "at line "

         else
            Set_Msg_Str ("at line ");
         end if;

         --  Output line number for reference

         Set_Msg_Int (Int (Get_Logical_Line_Number (Loc)));

         --  Deal with the instantiation case. We may have a reference to,
         --  e.g. a type, that is declared within a generic template, and
         --  what we are really referring to is the occurrence in an instance.
         --  In this case, the line number of the instantiation is also of
         --  interest, and we add a notation:

         --    , instance at xxx

         --  where xxx is a line number output using this same routine (and
         --  the recursion can go further if the instantiation is itself in
         --  a generic template).

         --  The flag location passed to us in this situation is indeed the
         --  line number within the template, but as described in Sinput.L
         --  (file sinput-l.ads, section "Handling Generic Instantiations")
         --  we can retrieve the location of the instantiation itself from
         --  this flag location value.

         --  Note: this processing is suppressed if Suppress_Instance_Location
         --  is set True. This is used to prevent redundant annotations of the
         --  location of the instantiation in the case where we are placing
         --  the messages on the instantiation in any case.

         if Instantiation (Sindex_Loc) /= No_Location
           and then not Suppress_Instance_Location
         then
            Set_Msg_Str (", instance ");
            Set_Msg_Insertion_Line_Number (Instantiation (Sindex_Loc), Flag);
         end if;
      end if;
   end Set_Msg_Insertion_Line_Number;

   ----------------------------
   -- Set_Msg_Insertion_Name --
   ----------------------------

   procedure Set_Msg_Insertion_Name is
   begin
      if Error_Msg_Name_1 = No_Name then
         null;

      elsif Error_Msg_Name_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank_Conditional;
         Get_Unqualified_Decoded_Name_String (Error_Msg_Name_1);

         --  Remove %s or %b at end. These come from unit names. If the
         --  caller wanted the (unit) or (body), then they would have used
         --  the $ insertion character. Certainly no error message should
         --  ever have %b or %s explicitly occurring.

         if Name_Len > 2
           and then Name_Buffer (Name_Len - 1) = '%'
           and then (Name_Buffer (Name_Len) = 'b'
                       or else
                     Name_Buffer (Name_Len) = 's')
         then
            Name_Len := Name_Len - 2;
         end if;

         --  Remove upper case letter at end, again, we should not be getting
         --  such names, and what we hope is that the remainder makes sense.

         if Name_Len > 1
           and then Name_Buffer (Name_Len) in 'A' .. 'Z'
         then
            Name_Len := Name_Len - 1;
         end if;

         --  If operator name or character literal name, just print it as is
         --  Also print as is if it ends in a right paren (case of x'val(nnn))

         if Name_Buffer (1) = '"'
           or else Name_Buffer (1) = '''
           or else Name_Buffer (Name_Len) = ')'
         then
            Set_Msg_Name_Buffer;

         --  Else output with surrounding quotes in proper casing mode

         else
            Set_Casing (Identifier_Casing (Flag_Source), Mixed_Case);
            Set_Msg_Quote;
            Set_Msg_Name_Buffer;
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignments ensure that the second and third percent
      --  insertion characters will correspond to the Error_Msg_Name_2 and
      --  Error_Msg_Name_3 as required.

      Error_Msg_Name_1 := Error_Msg_Name_2;
      Error_Msg_Name_2 := Error_Msg_Name_3;

   end Set_Msg_Insertion_Name;

   ----------------------------
   -- Set_Msg_Insertion_Node --
   ----------------------------

   procedure Set_Msg_Insertion_Node is
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

         --  Skip quotes for operator case

         if Nkind (Error_Msg_Node_1) in N_Op then
            Set_Msg_Node (Error_Msg_Node_1);

         else
            Set_Msg_Quote;
            Set_Qualification (Error_Msg_Qual_Level, Error_Msg_Node_1);
            Set_Msg_Node (Error_Msg_Node_1);
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignment ensures that a second ampersand insertion
      --  character will correspond to the Error_Msg_Node_2 parameter.

      Error_Msg_Node_1 := Error_Msg_Node_2;

   end Set_Msg_Insertion_Node;

   -------------------------------------
   -- Set_Msg_Insertion_Reserved_Name --
   -------------------------------------

   procedure Set_Msg_Insertion_Reserved_Name is
   begin
      Set_Msg_Blank_Conditional;
      Get_Name_String (Error_Msg_Name_1);
      Set_Msg_Quote;
      Set_Casing (Keyword_Casing (Flag_Source), All_Lower_Case);
      Set_Msg_Name_Buffer;
      Set_Msg_Quote;
   end Set_Msg_Insertion_Reserved_Name;

   -------------------------------------
   -- Set_Msg_Insertion_Reserved_Word --
   -------------------------------------

   procedure Set_Msg_Insertion_Reserved_Word
     (Text : String;
      J    : in out Integer)
   is
   begin
      Set_Msg_Blank_Conditional;
      Name_Len := 0;

      while J <= Text'Last and then Text (J) in 'A' .. 'Z' loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Text (J);
         J := J + 1;
      end loop;

      Set_Casing (Keyword_Casing (Flag_Source), All_Lower_Case);
      Set_Msg_Quote;
      Set_Msg_Name_Buffer;
      Set_Msg_Quote;
   end Set_Msg_Insertion_Reserved_Word;

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
         Set_Msg_Quote;
      end if;

      --  If the original type did not come from a predefined
      --  file, add the location where the type was defined.

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

   ----------------------------
   -- Set_Msg_Insertion_Uint --
   ----------------------------

   procedure Set_Msg_Insertion_Uint is
   begin
      Set_Msg_Blank;
      UI_Image (Error_Msg_Uint_1);

      for J in 1 .. UI_Image_Length loop
         Set_Msg_Char (UI_Image_Buffer (J));
      end loop;

      --  The following assignment ensures that a second carret insertion
      --  character will correspond to the Error_Msg_Uint_2 parameter.

      Error_Msg_Uint_1 := Error_Msg_Uint_2;
   end Set_Msg_Insertion_Uint;

   ---------------------------------
   -- Set_Msg_Insertion_Unit_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_Unit_Name is
   begin
      if Error_Msg_Unit_1 = No_Name then
         null;

      elsif Error_Msg_Unit_1 = Error_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Get_Unit_Name_String (Error_Msg_Unit_1);
         Set_Msg_Blank;
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignment ensures that a second percent insertion
      --  character will correspond to the Error_Msg_Unit_2 parameter.

      Error_Msg_Unit_1 := Error_Msg_Unit_2;

   end Set_Msg_Insertion_Unit_Name;

   -----------------
   -- Set_Msg_Int --
   -----------------

   procedure Set_Msg_Int (Line : Int) is
   begin
      if Line > 9 then
         Set_Msg_Int (Line / 10);
      end if;

      Set_Msg_Char (Character'Val (Character'Pos ('0') + (Line rem 10)));
   end Set_Msg_Int;

   -------------------------
   -- Set_Msg_Name_Buffer --
   -------------------------

   procedure Set_Msg_Name_Buffer is
   begin
      for J in 1 .. Name_Len loop
         Set_Msg_Char (Name_Buffer (J));
      end loop;
   end Set_Msg_Name_Buffer;

   ------------------
   -- Set_Msg_Node --
   ------------------

   procedure Set_Msg_Node (Node : Node_Id) is
      Ent : Entity_Id;
      Nam : Name_Id;

   begin
      if Nkind (Node) = N_Designator then
         Set_Msg_Node (Name (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Identifier (Node));
         return;

      elsif Nkind (Node) = N_Defining_Program_Unit_Name then
         Set_Msg_Node (Name (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Defining_Identifier (Node));
         return;

      elsif Nkind (Node) = N_Selected_Component then
         Set_Msg_Node (Prefix (Node));
         Set_Msg_Char ('.');
         Set_Msg_Node (Selector_Name (Node));
         return;
      end if;

      --  The only remaining possibilities are identifiers, defining
      --  identifiers, pragmas, and pragma argument associations, i.e.
      --  nodes that have a Chars field.

      --  Internal names generally represent something gone wrong. An exception
      --  is the case of internal type names, where we try to find a reasonable
      --  external representation for the external name

      if Is_Internal_Name (Chars (Node))
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

         Unwind_Internal_Type (Ent);
         Nam := Chars (Ent);

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
         Src_Loc : constant Source_Ptr := Sloc (Error_Msg_Node_1);
         Sbuffer : Source_Buffer_Ptr;
         Ref_Ptr : Integer;
         Src_Ptr : Source_Ptr;

      begin
         Ref_Ptr := 1;
         Src_Ptr := Src_Loc;

         --  Determine if the reference we are dealing with corresponds
         --  to text at the point of the error reference. This will often
         --  be the case for simple identifier references, and is the case
         --  where we can copy the spelling from the source.

         if Src_Loc /= No_Location
           and then Src_Loc > Standard_Location
         then
            Sbuffer := Source_Text (Get_Source_File_Index (Src_Loc));

            while Ref_Ptr <= Name_Len loop
               exit when
                 Fold_Lower (Sbuffer (Src_Ptr)) /=
                 Fold_Lower (Name_Buffer (Ref_Ptr));
               Ref_Ptr := Ref_Ptr + 1;
               Src_Ptr := Src_Ptr + 1;
            end loop;
         end if;

         --  If we get through the loop without a mismatch, then output
         --  the name the way it is spelled in the source program

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
      end;

      Set_Msg_Name_Buffer;
      Add_Class;

      --  Add 'Class if class wide type

      if Class_Flag then
         Set_Msg_Char (''');
         Get_Name_String (Name_Class);
         Set_Casing (Identifier_Casing (Flag_Source), Mixed_Case);
         Set_Msg_Name_Buffer;
      end if;
   end Set_Msg_Node;

   -------------------
   -- Set_Msg_Quote --
   -------------------

   procedure Set_Msg_Quote is
   begin
      if not Manual_Quote_Mode then
         Set_Msg_Char ('"');
      end if;
   end Set_Msg_Quote;

   -----------------
   -- Set_Msg_Str --
   -----------------

   procedure Set_Msg_Str (Text : String) is
   begin
      for J in Text'Range loop
         Set_Msg_Char (Text (J));
      end loop;
   end Set_Msg_Str;

   ------------------
   -- Set_Msg_Text --
   ------------------

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr) is
      C : Character;         -- Current character
      P : Natural;           -- Current index;

   begin
      Manual_Quote_Mode := False;
      Is_Unconditional_Msg := False;
      Msglen := 0;
      Flag_Source := Get_Source_File_Index (Flag);
      P := Text'First;

      while P <= Text'Last loop
         C := Text (P);
         P := P + 1;

         --  Check for insertion character

         if C = '%' then
            Set_Msg_Insertion_Name;

         elsif C = '$' then
            Set_Msg_Insertion_Unit_Name;

         elsif C = '{' then
            Set_Msg_Insertion_File_Name;

         elsif C = '}' then
            Set_Msg_Insertion_Type_Reference (Flag);

         elsif C = '*' then
            Set_Msg_Insertion_Reserved_Name;

         elsif C = '&' then
            Set_Msg_Insertion_Node;

         elsif C = '#' then
            Set_Msg_Insertion_Line_Number (Error_Msg_Sloc, Flag);

         elsif C = '\' then
            Continuation := True;

         elsif C = '@' then
            Set_Msg_Insertion_Column;

         elsif C = '^' then
            Set_Msg_Insertion_Uint;

         elsif C = '`' then
            Manual_Quote_Mode := not Manual_Quote_Mode;
            Set_Msg_Char ('"');

         elsif C = '!' then
            Is_Unconditional_Msg := True;

         elsif C = '?' then
            null;

         elsif C = ''' then
            Set_Msg_Char (Text (P));
            P := P + 1;

         --  Upper case letter (start of reserved word if 2 or more)

         elsif C in 'A' .. 'Z'
           and then P <= Text'Last
           and then Text (P) in 'A' .. 'Z'
         then
            P := P - 1;
            Set_Msg_Insertion_Reserved_Word (Text, P);

         --  Normal character with no special treatment

         else
            Set_Msg_Char (C);
         end if;

      end loop;
   end Set_Msg_Text;

   ------------------------------
   -- Set_Next_Non_Deleted_Msg --
   ------------------------------

   procedure Set_Next_Non_Deleted_Msg (E : in out Error_Msg_Id) is
   begin
      if E = No_Error_Msg then
         return;

      else
         loop
            E := Errors.Table (E).Next;
            exit when E = No_Error_Msg or else not Errors.Table (E).Deleted;
         end loop;
      end if;
   end Set_Next_Non_Deleted_Msg;

   ----------------
   -- Set_Posted --
   ----------------

   procedure Set_Posted (N : Node_Id) is
      P : Node_Id;

   begin
      --  We always set Error_Posted on the node itself

      Set_Error_Posted (N);

      --  If it is a subexpression, then set Error_Posted on parents
      --  up to and including the first non-subexpression construct. This
      --  helps avoid cascaded error messages within a single expression.

      P := N;
      loop
         P := Parent (P);
         exit when No (P);
         Set_Error_Posted (P);
         exit when Nkind (P) not in N_Subexpr;
      end loop;
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

   ---------------------------
   -- Set_Warnings_Mode_Off --
   ---------------------------

   procedure Set_Warnings_Mode_Off (Loc : Source_Ptr) is
   begin
      --  Don't bother with entries from instantiation copies, since we
      --  will already have a copy in the template, which is what matters

      if Instantiation (Get_Source_File_Index (Loc)) /= No_Location then
         return;
      end if;

      --  If last entry in table already covers us, this is a redundant
      --  pragma Warnings (Off) and can be ignored. This also handles the
      --  case where all warnings are suppressed by command line switch.

      if Warnings.Last >= Warnings.First
        and then Warnings.Table (Warnings.Last).Start <= Loc
        and then Loc <= Warnings.Table (Warnings.Last).Stop
      then
         return;

      --  Otherwise establish a new entry, extending from the location of
      --  the pragma to the end of the current source file. This ending
      --  point will be adjusted by a subsequent pragma Warnings (On).

      else
         Warnings.Increment_Last;
         Warnings.Table (Warnings.Last).Start := Loc;
         Warnings.Table (Warnings.Last).Stop :=
           Source_Last (Current_Source_File);
      end if;
   end Set_Warnings_Mode_Off;

   --------------------------
   -- Set_Warnings_Mode_On --
   --------------------------

   procedure Set_Warnings_Mode_On (Loc : Source_Ptr) is
   begin
      --  Don't bother with entries from instantiation copies, since we
      --  will already have a copy in the template, which is what matters

      if Instantiation (Get_Source_File_Index (Loc)) /= No_Location then
         return;
      end if;

      --  Nothing to do unless command line switch to suppress all warnings
      --  is off, and the last entry in the warnings table covers this
      --  pragma Warnings (On), in which case adjust the end point.

      if (Warnings.Last >= Warnings.First
           and then Warnings.Table (Warnings.Last).Start <= Loc
           and then Loc <= Warnings.Table (Warnings.Last).Stop)
        and then Warning_Mode /= Suppress
      then
         Warnings.Table (Warnings.Last).Stop := Loc;
      end if;
   end Set_Warnings_Mode_On;

   ----------------------
   -- Test_Warning_Msg --
   ----------------------

   procedure Test_Warning_Msg (Msg : String) is
   begin
      if Msg'Length > 7 and then Msg (1 .. 7) = "(style)" then
         Is_Warning_Msg := True;
         return;
      end if;

      for J in Msg'Range loop
         if Msg (J) = '?'
           and then (J = Msg'First or else Msg (J - 1) /= ''')
         then
            Is_Warning_Msg := True;
            return;
         end if;
      end loop;

      Is_Warning_Msg := False;
   end Test_Warning_Msg;

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

      --  The loop here deals with recursive types, we are trying to
      --  find a related entity that is not an implicit type. Note
      --  that the check with Old_Ent stops us from getting "stuck".
      --  Also, we don't output the "type derived from" message more
      --  than once in the case where we climb up multiple levels.

      loop
         Old_Ent := Ent;

         --  Implicit access type, use directly designated type

         if Is_Access_Type (Ent) then
            Set_Msg_Str ("access to ");
            Ent := Directly_Designated_Type (Ent);

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

         --  If this is a base type with a first named subtype, use the
         --  first named subtype instead. This is not quite accurate in
         --  all cases, but it makes too much noise to be accurate and
         --  add 'Base in all cases. Note that we only do this is the
         --  first named subtype is not itself an internal name. This
         --  avoids the obvious loop (subtype->basetype->subtype) which
         --  would otherwise occur!)

         elsif Present (Freeze_Node (Ent))
           and then Present (First_Subtype_Link (Freeze_Node (Ent)))
           and then
             not Is_Internal_Name
                   (Chars (First_Subtype_Link (Freeze_Node (Ent))))
         then
            Ent := First_Subtype_Link (Freeze_Node (Ent));

         --  Otherwise use root type

         else
            if not Derived then
               Buffer_Remove ("type ");

               --  Test for "subtype of type derived from" which seems
               --  excessive and is replaced by simply "type derived from"

               Buffer_Remove ("subtype of");

               --  Avoid duplication "type derived from type derived from"

               if not Buffer_Ends_With ("type derived from ") then
                  Set_Msg_Str ("type derived from ");
               end if;

               Derived := True;
            end if;

            Ent := Etype (Ent);
         end if;

         --  If we are stuck in a loop, get out and settle for the internal
         --  name after all. In this case we set to kill the message if it
         --  is not the first error message (we really try hard not to show
         --  the dirty laundry of the implementation to the poor user!)

         if Ent = Old_Ent then
            Kill_Message := True;
            exit;
         end if;

         --  Get out if we finally found a non-internal name to use

         exit when not Is_Internal_Name (Chars (Ent));
      end loop;

      if Mchar = '"' then
         Set_Msg_Char ('"');
      end if;

   end Unwind_Internal_Type;

   -------------------------
   -- Warnings_Suppressed --
   -------------------------

   function Warnings_Suppressed (Loc : Source_Ptr) return Boolean is
   begin
      for J in Warnings.First .. Warnings.Last loop
         if Warnings.Table (J).Start <= Loc
           and then Loc <= Warnings.Table (J).Stop
         then
            return True;
         end if;
      end loop;

      return False;
   end Warnings_Suppressed;

end Errout;
