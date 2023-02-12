------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E R R U T I L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1991-2023, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Err_Vars; use Err_Vars;
with Erroutc;  use Erroutc;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Scans;    use Scans;
with Sinput;   use Sinput;
with Stringt;  use Stringt;
with Stylesw;  use Stylesw;

package body Errutil is

   Errors_Must_Be_Ignored : Boolean := False;
   --  Set to True by procedure Set_Ignore_Errors (True), when calls to
   --  error message procedures should be ignored (when parsing irrelevant
   --  text in sources being preprocessed).

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Error_Msg_AP (Msg : String);
   --  Output a message just after the previous token

   procedure Output_Source_Line
     (L           : Physical_Line_Number;
      Sfile       : Source_File_Index;
      Errs        : Boolean;
      Source_Type : String);
   --  Outputs text of source line L, in file S, together with preceding line
   --  number, as described above for Output_Line_Number. The Errs parameter
   --  indicates if there are errors attached to the line, which forces
   --  listing on, even in the presence of pragma List (Off).

   procedure Set_Msg_Insertion_Column;
   --  Handle column number insertion (@ insertion character)

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr);
   --  Add a sequence of characters to the current message. The characters may
   --  be one of the special insertion characters (see documentation in spec).
   --  Flag is the location at which the error is to be posted, which is used
   --  to determine whether or not the # insertion needs a file name. The
   --  variables Msg_Buffer, Msglen, Is_Style_Msg, Is_Warning_Msg, and
   --  Is_Unconditional_Msg are set on return.

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

      --  Note that it is not worth worrying about special UTF_32 line
      --  terminator characters in this context, since this is only about
      --  error recovery anyway.

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

      --  Note that it is not worth worrying about special UTF_32 line
      --  terminator characters in this context, since this is only about
      --  error recovery anyway.

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

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (Msg : String; Flag_Location : Source_Ptr) is

      Next_Msg : Error_Msg_Id;
      --  Pointer to next message at insertion point

      Prev_Msg : Error_Msg_Id;
      --  Pointer to previous message at insertion point

      Sptr : Source_Ptr renames Flag_Location;
      --  Corresponds to the Sptr value in the error message object

      Optr : Source_Ptr renames Flag_Location;
      --  Corresponds to the Optr value in the error message object. Note that
      --  for this usage, Sptr and Optr always have the same value, since we do
      --  not have to worry about generic instantiations.

   begin
      if Errors_Must_Be_Ignored then
         return;
      end if;

      if Raise_Exception_On_Error /= 0 then
         raise Error_Msg_Exception;
      end if;

      Prescan_Message (Msg);
      Set_Msg_Text (Msg, Sptr);

      --  Kill continuation if parent message killed

      if Continuation and Last_Killed then
         return;
      end if;

      --  Return without doing anything if message is killed and this is not
      --  the first error message. The philosophy is that if we get a weird
      --  error message and we already have had a message, then we hope the
      --  weird message is a junk cascaded message

      --  Immediate return if warning message and warnings are suppressed.
      --  Note that style messages are not warnings for this purpose.

      if Is_Warning_Msg and then Warnings_Suppressed (Sptr) /= No_String then
         Cur_Msg := No_Error_Msg;
         return;
      end if;

      --  Otherwise build error message object for new message

      Errors.Append
        (New_Val =>
           (Text                => new String'(Msg_Buffer (1 .. Msglen)),
            Next                => No_Error_Msg,
            Prev                => No_Error_Msg,
            Sfile               => Get_Source_File_Index (Sptr),
            Sptr                => To_Span (Sptr),
            Optr                => To_Span (Optr),
            Insertion_Sloc      => No_Location,
            Line                => Get_Physical_Line_Number (Sptr),
            Col                 => Get_Column_Number (Sptr),
            Compile_Time_Pragma => Is_Compile_Time_Msg,
            Warn                => Is_Warning_Msg,
            Info                => Is_Info_Msg,
            Check               => Is_Check_Msg,
            Warn_Err            => Warning_Mode = Treat_As_Error,
            Warn_Runtime_Raise  => Is_Runtime_Raise,
            Warn_Chr            => Warning_Msg_Char,
            Style               => Is_Style_Msg,
            Serious             => Is_Serious_Error,
            Uncond              => Is_Unconditional_Msg,
            Msg_Cont            => Continuation,
            Deleted             => False,
            Node                => Empty));

      Cur_Msg  := Errors.Last;
      Prev_Msg := No_Error_Msg;
      Next_Msg := First_Error_Msg;

      while Next_Msg /= No_Error_Msg loop
         exit when
           Errors.Table (Cur_Msg).Sfile < Errors.Table (Next_Msg).Sfile;

         if Errors.Table (Cur_Msg).Sfile = Errors.Table (Next_Msg).Sfile then
            exit when Sptr < Errors.Table (Next_Msg).Sptr.Ptr;
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
      then
         --  Don't delete unconditional messages and at this stage, don't
         --  delete continuation lines (we attempted to delete those earlier
         --  if the parent message was deleted.

         if not Errors.Table (Cur_Msg).Uncond
           and then not Continuation
         then

            --  Don't delete if prev msg is warning and new msg is an error.
            --  This is because we don't want a real error masked by a warning.
            --  In all other cases (that is parse errors for the same line that
            --  are not unconditional) we do delete the message. This helps to
            --  avoid junk extra messages from cascaded parsing errors

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

      --  Bump appropriate statistics counts

      if Errors.Table (Cur_Msg).Info then

         --  Could be (usually is) both "info" and "warning"

         if Errors.Table (Cur_Msg).Warn then
            Warning_Info_Messages := Warning_Info_Messages + 1;
            Warnings_Detected := Warnings_Detected + 1;
         else
            Report_Info_Messages := Report_Info_Messages + 1;
         end if;

      elsif Errors.Table (Cur_Msg).Warn
        or else Errors.Table (Cur_Msg).Style
      then
         Warnings_Detected := Warnings_Detected + 1;

      elsif Errors.Table (Cur_Msg).Check then
         Check_Messages := Check_Messages + 1;

      else
         Total_Errors_Detected := Total_Errors_Detected + 1;

         if Errors.Table (Cur_Msg).Serious then
            Serious_Errors_Detected := Serious_Errors_Detected + 1;
         end if;
      end if;

   end Error_Msg;

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

   procedure Finalize (Source_Type : String := "project") is
      Cur      : Error_Msg_Id;
      Nxt      : Error_Msg_Id;
      E, F     : Error_Msg_Id;
      Err_Flag : Boolean;

   begin
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

      --  Brief Error mode

      if Brief_Output or (not Full_List and not Verbose_Mode) then
         E := First_Error_Msg;
         Set_Standard_Error;

         while E /= No_Error_Msg loop
            if not Errors.Table (E).Deleted then
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
         Write_Eol;

         --  First list initial main source file with its error messages

         for N in 1 .. Last_Source_Line (Main_Source_File) loop
            Err_Flag :=
              E /= No_Error_Msg
                and then Errors.Table (E).Line = N
                and then Errors.Table (E).Sfile = Main_Source_File;

            Output_Source_Line (N, Main_Source_File, Err_Flag, Source_Type);

            if Err_Flag then
               Output_Error_Msgs (E);

               Write_Eol;
            end if;
         end loop;

         --  Then output errors, if any, for subsidiary units

         while E /= No_Error_Msg
           and then Errors.Table (E).Sfile /= Main_Source_File
         loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line,
               Errors.Table (E).Sfile,
               True,
               Source_Type);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Verbose mode (error lines only with error flags)

      if Verbose_Mode then
         E := First_Error_Msg;

         --  Loop through error lines

         while E /= No_Error_Msg loop
            Write_Eol;
            Output_Source_Line
              (Errors.Table (E).Line,
               Errors.Table (E).Sfile,
               True,
               Source_Type);
            Output_Error_Msgs (E);
         end loop;
      end if;

      --  Output error summary if verbose or full list mode

      if Verbose_Mode or else Full_List then

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

         --  Historical note: Formerly, only the "# errors" suffix was sent
         --  to stderr, whereas "# lines:" appeared on stdout. This caused
         --  some problems on now-obsolete ports, but there seems to be no
         --  reason to revert this page since it would be incompatible.

         if Total_Errors_Detected + Warnings_Detected /= 0
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

         if Total_Errors_Detected = 0 then
            Write_Str ("No errors");

         elsif Total_Errors_Detected = 1 then
            Write_Str ("1 error");

         else
            Write_Int (Total_Errors_Detected);
            Write_Str (" errors");
         end if;

         if Warnings_Detected - Warning_Info_Messages /= 0 then
            Write_Str (", ");
            Write_Int (Warnings_Detected - Warning_Info_Messages);
            Write_Str (" warning");

            if Warnings_Detected - Warning_Info_Messages /= 1 then
               Write_Char ('s');
            end if;

            if Warning_Mode = Treat_As_Error then
               Write_Str (" (treated as error");

               if Warnings_Detected - Warning_Info_Messages /= 1 then
                  Write_Char ('s');
               end if;

               Write_Char (')');
            end if;
         end if;

         Write_Eol;
         Set_Standard_Output;
      end if;

      if Maximum_Messages /= 0 then
         if Warnings_Detected >= Maximum_Messages then
            Set_Standard_Error;
            Write_Line ("maximum number of warnings detected");
            Warning_Mode := Suppress;
         end if;

         if Total_Errors_Detected >= Maximum_Messages then
            Set_Standard_Error;
            Write_Line ("fatal error: maximum errors reached");
            Set_Standard_Output;
         end if;
      end if;

      --  Even though Warning_Info_Messages are a subclass of warnings, they
      --  must not be treated as errors when -gnatwe is in effect.

      if Warning_Mode = Treat_As_Error then
         Total_Errors_Detected :=
           Total_Errors_Detected + Warnings_Detected - Warning_Info_Messages;
         Warnings_Detected := Warning_Info_Messages;
      end if;

      --  Prevent displaying the same messages again in the future

      First_Error_Msg := No_Error_Msg;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Errors.Init;
      First_Error_Msg := No_Error_Msg;
      Last_Error_Msg  := No_Error_Msg;
      Serious_Errors_Detected := 0;
      Total_Errors_Detected := 0;
      Warnings_Detected := 0;
      Warning_Info_Messages := 0;
      Report_Info_Messages := 0;
      Cur_Msg := No_Error_Msg;

      --  Initialize warnings table, if all warnings are suppressed, supply
      --  an initial dummy entry covering all possible source locations.

      Warnings.Init;

      if Warning_Mode = Suppress then
         Warnings.Append
           (New_Val =>
              (Start  => Source_Ptr'First,
               Stop   => Source_Ptr'Last,
               Reason => Null_String_Id));
      end if;
   end Initialize;

   ------------------------
   -- Output_Source_Line --
   ------------------------

   procedure Output_Source_Line
     (L           : Physical_Line_Number;
      Sfile       : Source_File_Index;
      Errs        : Boolean;
      Source_Type : String)
   is
      S : Source_Ptr;
      C : Character;

      Line_Number_Output : Boolean := False;
      --  Set True once line number is output

   begin
      if Sfile /= Current_Error_Source_File then
         Write_Str ("==============Error messages for ");
         Write_Str (Source_Type);
         Write_Str (" file: ");
         Write_Name (Full_File_Name (Sfile));
         Write_Eol;
         Current_Error_Source_File := Sfile;
      end if;

      if Errs then
         Output_Line_Number (Physical_To_Logical (L, Sfile));
         Line_Number_Output := True;
      end if;

      S := Line_Start (L, Sfile);

      loop
         C := Source_Text (Sfile) (S);
         exit when C = ASCII.LF or else C = ASCII.CR or else C = EOF;

         if Errs then
            Write_Char (C);
         end if;

         S := S + 1;
      end loop;

      if Line_Number_Output then
         Write_Eol;
      end if;
   end Output_Source_Line;

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

   ------------------
   -- Set_Msg_Text --
   ------------------

   procedure Set_Msg_Text (Text : String; Flag : Source_Ptr) is
      C : Character; -- Current character
      P : Natural;   -- Current index;

   begin
      Manual_Quote_Mode := False;
      Msglen := 0;
      Flag_Source := Get_Source_File_Index (Flag);
      P := Text'First;

      while P <= Text'Last loop
         C := Text (P);
         P := P + 1;

         --  Check for insertion character

         if C = '%' then
            if P <= Text'Last and then Text (P) = '%' then
               P := P + 1;
               Set_Msg_Insertion_Name_Literal;
            else
               Set_Msg_Insertion_Name;
            end if;

         elsif C = '$' then

            --  '$' is ignored

            null;

         elsif C = '{' then
            Set_Msg_Insertion_File_Name;

         elsif C = '}' then

            --  '}' is ignored

            null;

         elsif C = '*' then
            Set_Msg_Insertion_Reserved_Name;

         elsif C = '&' then

            --  '&' is ignored

            null;

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
            null;

         elsif C = '?' then
            null;

         elsif C = '<' then
            null;

         elsif C = '|' then
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

         elsif C = '~' then
            Set_Msg_Str (Error_Msg_String (1 .. Error_Msg_Strlen));

         --  Normal character with no special treatment

         else
            Set_Msg_Char (C);
         end if;

      end loop;
   end Set_Msg_Text;

end Errutil;
