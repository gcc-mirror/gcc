------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E R R O U T C                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2022, Free Software Foundation, Inc.         --
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

--  Warning: Error messages can be generated during Gigi processing by direct
--  calls to error message routines, so it is essential that the processing
--  in this body be consistent with the requirements for the Gigi processing
--  environment, and that in particular, no disallowed table expansion is
--  allowed to occur.

with Atree;    use Atree;
with Casing;   use Casing;
with Csets;    use Csets;
with Debug;    use Debug;
with Err_Vars; use Err_Vars;
with Fname;    use Fname;
with Namet;    use Namet;
with Opt;      use Opt;
with Output;   use Output;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Targparm;
with Uintp;    use Uintp;
with Widechar; use Widechar;

package body Erroutc is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Sloc_In_Range (Loc, Start, Stop : Source_Ptr) return Boolean;
   --  Return whether Loc is in the range Start .. Stop, taking instantiation
   --  locations of Loc into account. This is useful for suppressing warnings
   --  from generic instantiations by using pragma Warnings around generic
   --  instances, as needed in GNATprove.

   ---------------
   -- Add_Class --
   ---------------

   procedure Add_Class is
   begin
      if Class_Flag then
         Class_Flag := False;
         Set_Msg_Char (''');
         Get_Name_String (Name_Class);
         Set_Casing (Identifier_Casing (Flag_Source));
         Set_Msg_Name_Buffer;
      end if;
   end Add_Class;

   ----------------------
   -- Buffer_Ends_With --
   ----------------------

   function Buffer_Ends_With (C : Character) return Boolean is
   begin
      return Msglen > 0 and then Msg_Buffer (Msglen) = C;
   end Buffer_Ends_With;

   function Buffer_Ends_With (S : String) return Boolean is
      Len : constant Natural := S'Length;
   begin
      return Msglen > Len
        and then Msg_Buffer (Msglen - Len) = ' '
        and then Msg_Buffer (Msglen - Len + 1 .. Msglen) = S;
   end Buffer_Ends_With;

   -------------------
   -- Buffer_Remove --
   -------------------

   procedure Buffer_Remove (C : Character) is
   begin
      if Buffer_Ends_With (C) then
         Msglen := Msglen - 1;
      end if;
   end Buffer_Remove;

   procedure Buffer_Remove (S : String) is
   begin
      if Buffer_Ends_With (S) then
         Msglen := Msglen - S'Length;
      end if;
   end Buffer_Remove;

   -----------------------------
   -- Check_Duplicate_Message --
   -----------------------------

   procedure Check_Duplicate_Message (M1, M2 : Error_Msg_Id) is
      L1, L2 : Error_Msg_Id;
      N1, N2 : Error_Msg_Id;

      procedure Delete_Msg (Delete, Keep : Error_Msg_Id);
      --  Called to delete message Delete, keeping message Keep. Marks msg
      --  Delete and all its continuations with deleted flag set to True.
      --  Also makes sure that for the error messages that are retained the
      --  preferred message is the one retained (we prefer the shorter one in
      --  the case where one has an Instance tag). Note that we always know
      --  that Keep has at least as many continuations as Delete (since we
      --  always delete the shorter sequence).

      ----------------
      -- Delete_Msg --
      ----------------

      procedure Delete_Msg (Delete, Keep : Error_Msg_Id) is
         D, K : Error_Msg_Id;

      begin
         D := Delete;
         K := Keep;

         loop
            Errors.Table (D).Deleted := True;

            --  Adjust error message count

            if Errors.Table (D).Info then

               if Errors.Table (D).Warn then
                  Warning_Info_Messages := Warning_Info_Messages - 1;
                  Warnings_Detected := Warnings_Detected - 1;
               else
                  Report_Info_Messages := Report_Info_Messages - 1;
               end if;

            elsif Errors.Table (D).Warn or else Errors.Table (D).Style then
               Warnings_Detected := Warnings_Detected - 1;

               --  Note: we do not need to decrement Warnings_Treated_As_Errors
               --  because this only gets incremented if we actually output the
               --  message, which we won't do if we are deleting it here!

            elsif Errors.Table (D).Check then
               Check_Messages := Check_Messages - 1;

            else
               Total_Errors_Detected := Total_Errors_Detected - 1;

               if Errors.Table (D).Serious then
                  Serious_Errors_Detected := Serious_Errors_Detected - 1;
               end if;
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

         --  If M2 continuations have run out, we delete M2

         elsif N2 = No_Error_Msg or else not Errors.Table (N2).Msg_Cont then
            Delete_Msg (M2, M1);
            return;

         --  Otherwise see if continuations are the same, if not, keep both
         --  sequences, a curious case, but better to keep everything.

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
      Warnings_Count : constant Int
         := Warnings_Detected - Warning_Info_Messages;
   begin
      if Total_Errors_Detected /= 0 then
         return True;

      elsif Warnings_Treated_As_Errors /= 0 then
         return True;

      --  We should never treat warnings that originate from a
      --  Compile_Time_Warning pragma as an error. Warnings_Count is the sum
      --  of both "normal" and Compile_Time_Warning warnings. This means that
      --  there are only one or more non-Compile_Time_Warning warnings when
      --  Warnings_Count is greater than Count_Compile_Time_Pragma_Warnings.

      elsif Warning_Mode = Treat_As_Error
         and then Warnings_Count > Count_Compile_Time_Pragma_Warnings
      then
         return True;
      end if;

      return False;
   end Compilation_Errors;

   ----------------------------------------
   -- Count_Compile_Time_Pragma_Warnings  --
   ----------------------------------------

   function Count_Compile_Time_Pragma_Warnings return Int is
      Result : Int := 0;
   begin
      for J in 1 .. Errors.Last loop
         begin
            if Errors.Table (J).Warn
               and then Errors.Table (J).Compile_Time_Pragma
               and then not Errors.Table (J).Deleted
            then
               Result := Result + 1;
            end if;
         end;
      end loop;
      return Result;
   end Count_Compile_Time_Pragma_Warnings;

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
      w ("  Prev     = ", Int (E.Prev));
      w ("  Sfile    = ", Int (E.Sfile));

      Write_Str
        ("  Sptr     = ");
      Write_Location (E.Sptr.Ptr);  --  ??? Do not write the full span for now
      Write_Eol;

      Write_Str
        ("  Optr     = ");
      Write_Location (E.Optr);
      Write_Eol;

      w ("  Line     = ", Int (E.Line));
      w ("  Col      = ", Int (E.Col));
      w ("  Warn     = ", E.Warn);
      w ("  Warn_Err = ", E.Warn_Err);
      w ("  Warn_Chr = '" & E.Warn_Chr & ''');
      w ("  Style    = ", E.Style);
      w ("  Serious  = ", E.Serious);
      w ("  Uncond   = ", E.Uncond);
      w ("  Msg_Cont = ", E.Msg_Cont);
      w ("  Deleted  = ", E.Deleted);
      w ("  Node     = ", Int (E.Node));

      Write_Eol;
   end dmsg;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location (E : Error_Msg_Id) return Source_Ptr is
   begin
      return Errors.Table (E).Sptr.Ptr;
   end Get_Location;

   ----------------
   -- Get_Msg_Id --
   ----------------

   function Get_Msg_Id return Error_Msg_Id is
   begin
      return Cur_Msg;
   end Get_Msg_Id;

   ------------------------
   -- Get_Warning_Option --
   ------------------------

   function Get_Warning_Option (Id : Error_Msg_Id) return String is
      Warn     : constant Boolean         := Errors.Table (Id).Warn;
      Warn_Chr : constant String (1 .. 2) := Errors.Table (Id).Warn_Chr;
   begin
      if Warn and then Warn_Chr /= "  " and then Warn_Chr (1) /= '?' then
         if Warn_Chr = "$ " then
            return "-gnatel";
         elsif Warn_Chr (2) = ' ' then
            return "-gnatw" & Warn_Chr (1);
         else
            return "-gnatw" & Warn_Chr;
         end if;
      end if;
      return "";
   end Get_Warning_Option;

   ---------------------
   -- Get_Warning_Tag --
   ---------------------

   function Get_Warning_Tag (Id : Error_Msg_Id) return String is
      Warn     : constant Boolean         := Errors.Table (Id).Warn;
      Warn_Chr : constant String (1 .. 2) := Errors.Table (Id).Warn_Chr;
      Option   : constant String          := Get_Warning_Option (Id);
   begin
      if Warn then
         if Warn_Chr = "? " then
            return "[enabled by default]";
         elsif Warn_Chr = "* " then
            return "[restriction warning]";
         elsif Option /= "" then
            return "[" & Option & "]";
         end if;
      end if;

      return "";
   end Get_Warning_Tag;

   -------------
   -- Matches --
   -------------

   function Matches (S : String; P : String) return Boolean is
      Slast : constant Natural := S'Last;
      PLast : constant Natural := P'Last;

      SPtr : Natural := S'First;
      PPtr : Natural := P'First;

   begin
      --  Loop advancing through characters of string and pattern

      SPtr := S'First;
      PPtr := P'First;
      loop
         --  Return True if pattern is a single asterisk

         if PPtr = PLast and then P (PPtr) = '*' then
            return True;

         --  Return True if both pattern and string exhausted

         elsif PPtr > PLast and then SPtr > Slast then
            return True;

         --  Return False, if one exhausted and not the other

         elsif PPtr > PLast or else SPtr > Slast then
            return False;

         --  Case where pattern starts with asterisk

         elsif P (PPtr) = '*' then

            --  Try all possible starting positions in S for match with the
            --  remaining characters of the pattern. This is the recursive
            --  call that implements the scanner backup.

            for J in SPtr .. Slast loop
               if Matches (S (J .. Slast), P (PPtr + 1 .. PLast)) then
                  return True;
               end if;
            end loop;

            return False;

         --  Dealt with end of string and *, advance if we have a match

         elsif Fold_Lower (S (SPtr)) = Fold_Lower (P (PPtr)) then
            SPtr := SPtr + 1;
            PPtr := PPtr + 1;

         --  If first characters do not match, that's decisive

         else
            return False;
         end if;
      end loop;
   end Matches;

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
         if Errors.Table (T).Sptr.Ptr > Errors.Table (E).Sptr.Ptr then
            Mult_Flags := True;
         end if;

         Set_Next_Non_Deleted_Msg (T);
      end loop;

      --  Output the error flags. The circuit here makes sure that the tab
      --  characters in the original line are properly accounted for. The
      --  eight blanks at the start are to match the line number.

      if not Debug_Flag_2 then
         Write_Str ("        ");
         P := Line_Start (Errors.Table (E).Sptr.Ptr);
         Flag_Num := 1;

         --  Loop through error messages for this line to place flags

         T := S;
         while T /= No_Error_Msg
           and then Errors.Table (T).Line = Errors.Table (E).Line
           and then Errors.Table (T).Sfile = Errors.Table (E).Sfile
         loop
            declare
               Src : Source_Buffer_Ptr
                       renames Source_Text (Errors.Table (T).Sfile);

            begin
               --  Loop to output blanks till current flag position

               while P < Errors.Table (T).Sptr.Ptr loop

                  --  Horizontal tab case, just echo the tab

                  if Src (P) = ASCII.HT then
                     Write_Char (ASCII.HT);
                     P := P + 1;

                  --  Deal with wide character case, but don't include brackets
                  --  notation in this circuit, since we know that this will
                  --  display unencoded (no one encodes brackets notation).

                  elsif Src (P) /= '['
                    and then Is_Start_Of_Wide_Char (Src, P)
                  then
                     Skip_Wide (Src, P);
                     Write_Char (' ');

                  --  Normal non-wide character case (or bracket)

                  else
                     P := P + 1;
                     Write_Char (' ');
                  end if;
               end loop;

               --  Output flag (unless already output, this happens if more
               --  than one error message occurs at the same flag position).

               if P = Errors.Table (T).Sptr.Ptr then
                  if (Flag_Num = 1 and then not Mult_Flags)
                    or else Flag_Num > 9
                  then
                     Write_Char ('|');
                  else
                     Write_Char
                       (Character'Val (Character'Pos ('0') + Flag_Num));
                  end if;

                  --  Skip past the corresponding source text character

                  --  Horizontal tab case, we output a flag at the tab position
                  --  so now we output a tab to match up with the text.

                  if Src (P) = ASCII.HT then
                     Write_Char (ASCII.HT);
                     P := P + 1;

                  --  Skip wide character other than left bracket

                  elsif Src (P) /= '['
                    and then Is_Start_Of_Wide_Char (Src, P)
                  then
                     Skip_Wide (Src, P);

                  --  Skip normal non-wide character case (or bracket)

                  else
                     P := P + 1;
                  end if;
               end if;
            end;

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
      Offs : constant Nat := Column - 1;
      --  Offset to start of message, used for continuations

      Max : Integer;
      --  Maximum characters to output on next line

      Length : Nat;
      --  Maximum total length of lines

      E_Msg : Error_Msg_Object renames Errors.Table (E);
      Text  : constant String_Ptr := E_Msg.Text;
      Ptr   : Natural;
      Split : Natural;
      Start : Natural;
      Tag : constant String := Get_Warning_Tag (E);
      Txt : String_Ptr;
      Len : Natural;

   begin
      --  Postfix warning tag to message if needed

      if Tag /= "" and then Warning_Doc_Switch then
         if Include_Subprogram_In_Messages then
            Txt :=
              new String'
                (Subprogram_Name_Ptr (E_Msg.Node) &
                 ": " & Text.all & ' ' & Tag);
         else
            Txt := new String'(Text.all & ' ' & Tag);
         end if;

      elsif Include_Subprogram_In_Messages
        and then (E_Msg.Warn or else E_Msg.Style)
      then
         Txt :=
           new String'(Subprogram_Name_Ptr (E_Msg.Node) & ": " & Text.all);
      else
         Txt := Text;
      end if;

      --  If -gnatdF is used, continuation messages follow the main message
      --  with only an indentation of two space characters, without repeating
      --  any prefix.

      if Debug_Flag_FF and then E_Msg.Msg_Cont then
         null;

      --  For info messages, prefix message with "info: "

      elsif E_Msg.Info then
         Txt := new String'(SGR_Note & "info: " & SGR_Reset & Txt.all);

      --  Warning treated as error

      elsif E_Msg.Warn_Err then

      --  We prefix with "error:" rather than warning: and postfix
      --  [warning-as-error] at the end.

         Warnings_Treated_As_Errors := Warnings_Treated_As_Errors + 1;
         Txt := new String'(SGR_Error & "error: " & SGR_Reset
                            & Txt.all & " [warning-as-error]");

      --  Normal warning, prefix with "warning: "

      elsif E_Msg.Warn then
         Txt := new String'(SGR_Warning & "warning: " & SGR_Reset & Txt.all);

      --  No prefix needed for style message, "(style)" is there already,
      --  although not necessarily in first position if -gnatdJ is used.

      elsif E_Msg.Style then
         if Txt (Txt'First .. Txt'First + 6) = "(style)" then
            Txt := new String'(SGR_Warning & "(style)" & SGR_Reset
                               & Txt (Txt'First + 7 .. Txt'Last));
         end if;

      --  No prefix needed for check message, severity is there already

      elsif E_Msg.Check then

         --  The message format is "severity: ..."
         --
         --  Enclose the severity with an SGR control string if requested

         if Use_SGR_Control then
            declare
               Msg   : String renames Text.all;
               Colon : Natural := 0;
            begin
               --  Find first colon

               for J in Msg'Range loop
                  if Msg (J) = ':' then
                     Colon := J;
                     exit;
                  end if;
               end loop;

               pragma Assert (Colon > 0);

               Txt := new String'(SGR_Error
                                  & Msg (Msg'First .. Colon)
                                  & SGR_Reset
                                  & Msg (Colon + 1 .. Msg'Last));
            end;
         end if;

      --  All other cases, add "error: " if unique error tag set

      elsif Opt.Unique_Error_Tag then
         Txt := new String'(SGR_Error & "error: " & SGR_Reset & Txt.all);
      end if;

      --  Set error message line length and length of message

      if Error_Msg_Line_Length = 0 then
         Length := Nat'Last;
      else
         Length := Error_Msg_Line_Length;
      end if;

      Max := Integer (Length - Column + 1);
      Len := Txt'Length;

      --  Here we have to split the message up into multiple lines

      Ptr := 1;
      loop
         --  Make sure we do not have ludicrously small line

         Max := Integer'Max (Max, 20);

         --  If remaining text fits, output it respecting LF and we are done

         if Len - Ptr < Max then
            for J in Ptr .. Len loop
               if Txt (J) = ASCII.LF then
                  Write_Eol;
                  Write_Spaces (Offs);
               else
                  Write_Char (Txt (J));
               end if;
            end loop;

            return;

         --  Line does not fit

         else
            Start := Ptr;

            --  First scan forward looking for a hard end of line

            for Scan in Ptr .. Ptr + Max - 1 loop
               if Txt (Scan) = ASCII.LF then
                  Split := Scan - 1;
                  Ptr := Scan + 1;
                  goto Continue;
               end if;
            end loop;

            --  Otherwise scan backwards looking for a space

            for Scan in reverse Ptr .. Ptr + Max - 1 loop
               if Txt (Scan) = ' ' then
                  Split := Scan - 1;
                  Ptr := Scan + 1;
                  goto Continue;
               end if;
            end loop;

            --  If we fall through, no space, so split line arbitrarily

            Split := Ptr + Max - 1;
            Ptr := Split + 1;
         end if;

         <<Continue>>
         if Start <= Split then
            Write_Line (Txt (Start .. Split));
            Write_Spaces (Offs);
         end if;

         Max := Integer (Length - Column + 1);
      end loop;
   end Output_Msg_Text;

   ---------------------
   -- Prescan_Message --
   ---------------------

   procedure Prescan_Message (Msg : String) is
      J : Natural;

      function Parse_Message_Class return String;
      --  Convert the warning insertion sequence to a warning class represented
      --  as a length-two string padded, if necessary, with spaces.
      --  Return the Message class and set the iterator J to the character
      --  following the sequence.
      --  Raise a Program_Error if the insertion sequence is not valid.

      -------------------------
      -- Parse_Message_Class --
      -------------------------

      function Parse_Message_Class return String is
         C : constant Character := Msg (J - 1);
         Message_Class : String (1 .. 2) := "  ";
      begin
         if J <= Msg'Last and then Msg (J) = C then
            Message_Class := "? ";
            J := J + 1;

         elsif J < Msg'Last and then Msg (J + 1) = C
           and then Msg (J) in 'a' .. 'z' | '*' | '$'
         then
            Message_Class := Msg (J) & " ";
            J := J + 2;

         elsif J + 1 < Msg'Last and then Msg (J + 2) = C
           and then Msg (J) in '.' | '_'
           and then Msg (J + 1) in 'a' .. 'z'
         then
            Message_Class := Msg (J .. J + 1);
            J := J + 3;
         elsif (J < Msg'Last and then Msg (J + 1) = C) or else
            (J + 1 < Msg'Last and then Msg (J + 2) = C)
         then
            raise Program_Error;
         end if;

         --  In any other cases, this is not a warning insertion sequence
         --  and the default "  " value is returned.

         return Message_Class;
      end Parse_Message_Class;

   --  Start of processing for Prescan_Message

   begin
      --  Nothing to do for continuation line, unless -gnatdF is set

      if not Debug_Flag_FF and then Msg (Msg'First) = '\' then
         return;

      --  Some global variables are not set for continuation messages, as they
      --  only make sense for the initial message.

      elsif Msg (Msg'First) /= '\' then

         --  Set initial values of globals (may be changed during scan)

         Is_Serious_Error     := True;
         Is_Unconditional_Msg := False;
         Is_Warning_Msg       := False;
         Is_Runtime_Raise     := False;

         --  Check style message

         Is_Style_Msg :=
           Msg'Length > 7
             and then Msg (Msg'First .. Msg'First + 6) = "(style)";

         --  Check info message

         Is_Info_Msg :=
           Msg'Length > 6
             and then Msg (Msg'First .. Msg'First + 5) = "info: ";

         --  Check check message

         Is_Check_Msg :=
           (Msg'Length > 8
             and then Msg (Msg'First .. Msg'First + 7) = "medium: ")
           or else
           (Msg'Length > 6
             and then Msg (Msg'First .. Msg'First + 5) = "high: ")
           or else
           (Msg'Length > 5
             and then Msg (Msg'First .. Msg'First + 4) = "low: ");
      end if;

      Has_Double_Exclam  := False;
      Has_Insertion_Line := False;

      --  Loop through message looking for relevant insertion sequences

      J := Msg'First;
      while J <= Msg'Last loop

         --  If we have a quote, don't look at following character

         if Msg (J) = ''' then
            J := J + 2;

         --  Warning message (? or < insertion sequence)

         elsif Msg (J) = '?' or else Msg (J) = '<' then
            Is_Warning_Msg := Msg (J) = '?' or else Error_Msg_Warn;
            J := J + 1;

            if Is_Warning_Msg then
               Warning_Msg_Char := Parse_Message_Class;
            end if;

            --  Bomb if untagged warning message. This code can be uncommented
            --  for debugging when looking for untagged warning messages.

            --  if Is_Warning_Msg and then Warning_Msg_Char = ' ' then
            --     raise Program_Error;
            --  end if;

         --  Unconditional message (! insertion)

         elsif Msg (J) = '!' then
            Is_Unconditional_Msg := True;
            J := J + 1;

            if J <= Msg'Last and then Msg (J) = '!' then
               Has_Double_Exclam := True;
               J := J + 1;
            end if;

         --  Insertion line (# insertion)

         elsif Msg (J) = '#' then
            Has_Insertion_Line := True;
            J := J + 1;

         --  Non-serious error (| insertion)

         elsif Msg (J) = '|' then
            Is_Serious_Error := False;
            J := J + 1;

         else
            J := J + 1;
         end if;
      end loop;

      if Is_Info_Msg or Is_Warning_Msg or Is_Style_Msg or Is_Check_Msg then
         Is_Serious_Error := False;
      end if;
   end Prescan_Message;

   --------------------
   -- Purge_Messages --
   --------------------

   procedure Purge_Messages (From : Source_Ptr; To : Source_Ptr) is
      E : Error_Msg_Id;

      function To_Be_Purged (E : Error_Msg_Id) return Boolean;
      --  Returns True for a message that is to be purged. Also adjusts
      --  error counts appropriately.

      ------------------
      -- To_Be_Purged --
      ------------------

      function To_Be_Purged (E : Error_Msg_Id) return Boolean is
      begin
         if E /= No_Error_Msg
           and then Errors.Table (E).Sptr.Ptr > From
           and then Errors.Table (E).Sptr.Ptr < To
         then
            if Errors.Table (E).Warn or else Errors.Table (E).Style then
               Warnings_Detected := Warnings_Detected - 1;

            else
               Total_Errors_Detected := Total_Errors_Detected - 1;

               if Errors.Table (E).Serious then
                  Serious_Errors_Detected := Serious_Errors_Detected - 1;
               end if;
            end if;

            return True;

         else
            return False;
         end if;
      end To_Be_Purged;

   --  Start of processing for Purge_Messages

   begin
      while To_Be_Purged (First_Error_Msg) loop
         First_Error_Msg := Errors.Table (First_Error_Msg).Next;
      end loop;

      E := First_Error_Msg;
      while E /= No_Error_Msg loop
         while To_Be_Purged (Errors.Table (E).Next) loop
            Errors.Table (E).Next :=
              Errors.Table (Errors.Table (E).Next).Next;
         end loop;

         E := Errors.Table (E).Next;
      end loop;
   end Purge_Messages;

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
        and then Msg_Buffer (Msglen) /= '-'
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

   ---------------------------------
   -- Set_Msg_Insertion_File_Name --
   ---------------------------------

   procedure Set_Msg_Insertion_File_Name is
   begin
      if Error_Msg_File_1 = No_File then
         null;

      elsif Error_Msg_File_1 = Error_File_Name then
         Set_Msg_Blank;
         Set_Msg_Str ("<error>");

      else
         Set_Msg_Blank;
         Get_Name_String (Error_Msg_File_1);
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;

      --  The following assignments ensure that the second and third {
      --  insertion characters will correspond to the Error_Msg_File_2
      --  and Error_Msg_File_3 values.

      Error_Msg_File_1 := Error_Msg_File_2;
      Error_Msg_File_2 := Error_Msg_File_3;
   end Set_Msg_Insertion_File_Name;

   -----------------------------------
   -- Set_Msg_Insertion_Line_Number --
   -----------------------------------

   procedure Set_Msg_Insertion_Line_Number (Loc, Flag : Source_Ptr) is
      Sindex_Loc  : Source_File_Index;
      Sindex_Flag : Source_File_Index;
      Fname       : File_Name_Type;
      Int_File    : Boolean;

      procedure Set_At;
      --  Outputs "at " unless last characters in buffer are " from ". Certain
      --  messages read better with from than at.

      ------------
      -- Set_At --
      ------------

      procedure Set_At is
      begin
         if Msglen < 6
           or else Msg_Buffer (Msglen - 5 .. Msglen) /= " from "
         then
            Set_Msg_Str ("at ");
         end if;
      end Set_At;

   --  Start of processing for Set_Msg_Insertion_Line_Number

   begin
      Set_Msg_Blank;

      if Loc = No_Location then
         Set_At;
         Set_Msg_Str ("unknown location");

      elsif Loc = System_Location then
         Set_Msg_Str ("in package System");
         Set_Msg_Insertion_Run_Time_Name;

      elsif Loc = Standard_Location then
         Set_Msg_Str ("in package Standard");

      elsif Loc = Standard_ASCII_Location then
         Set_Msg_Str ("in package Standard.ASCII");

      else
         --  Add "at file-name:" if reference is to other than the source
         --  file in which the error message is placed. Note that we check
         --  full file names, rather than just the source indexes, to
         --  deal with generic instantiations from the current file.

         Sindex_Loc  := Get_Source_File_Index (Loc);
         Sindex_Flag := Get_Source_File_Index (Flag);

         if Full_File_Name (Sindex_Loc) /= Full_File_Name (Sindex_Flag) then
            Set_At;
            Fname := Reference_Name (Get_Source_File_Index (Loc));
            Int_File := Is_Internal_File_Name (Fname);
            Get_Name_String (Fname);
            Set_Msg_Name_Buffer;

            if not (Int_File and Debug_Flag_Dot_K) then
               Set_Msg_Char (':');
               Set_Msg_Int (Int (Get_Logical_Line_Number (Loc)));
            end if;

         --  If in current file, add text "at line "

         else
            Set_At;
            Set_Msg_Str ("line ");
            Set_Msg_Int (Int (Get_Logical_Line_Number (Loc)));
         end if;

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

         if Name_Len > 1 and then Name_Buffer (Name_Len) in 'A' .. 'Z' then
            Name_Len := Name_Len - 1;
         end if;

         --  If operator name or character literal name, just print it as is.
         --  Also print as is if it ends in a right paren (case of x'val(nnn)).

         if Name_Buffer (1) = '"'
           or else Name_Buffer (1) = '''
           or else Name_Buffer (Name_Len) = ')'
         then
            Set_Msg_Name_Buffer;

         --  Else output with surrounding quotes in proper casing mode

         else
            Set_Casing (Identifier_Casing (Flag_Source));
            Set_Msg_Quote;
            Set_Msg_Name_Buffer;
            Set_Msg_Quote;
         end if;
      end if;

      --  The following assignments ensure that other percent insertion
      --  characters will correspond to their appropriate Error_Msg_Name_#
      --  values as required.

      Error_Msg_Name_1 := Error_Msg_Name_2;
      Error_Msg_Name_2 := Error_Msg_Name_3;
      Error_Msg_Name_3 := Error_Msg_Name_4;
      Error_Msg_Name_4 := Error_Msg_Name_5;
      Error_Msg_Name_5 := Error_Msg_Name_6;
   end Set_Msg_Insertion_Name;

   ------------------------------------
   -- Set_Msg_Insertion_Name_Literal --
   ------------------------------------

   procedure Set_Msg_Insertion_Name_Literal is
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

      --  The following assignments ensure that other percent insertion
      --  characters will correspond to their appropriate Error_Msg_Name_#
      --  values as required.

      Error_Msg_Name_1 := Error_Msg_Name_2;
      Error_Msg_Name_2 := Error_Msg_Name_3;
      Error_Msg_Name_3 := Error_Msg_Name_4;
      Error_Msg_Name_4 := Error_Msg_Name_5;
      Error_Msg_Name_5 := Error_Msg_Name_6;
   end Set_Msg_Insertion_Name_Literal;

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
         Add_Char_To_Name_Buffer (Text (J));
         J := J + 1;
      end loop;

      --  Here is where we make the special exception for RM

      if Name_Len = 2 and then Name_Buffer (1 .. 2) = "RM" then
         Set_Msg_Name_Buffer;

      --  We make a similar exception for SPARK

      elsif Name_Len = 5 and then Name_Buffer (1 .. 5) = "SPARK" then
         Set_Msg_Name_Buffer;

      --  Neither RM nor SPARK: case appropriately and add surrounding quotes

      else
         Set_Casing (Keyword_Casing (Flag_Source), All_Lower_Case);
         Set_Msg_Quote;
         Set_Msg_Name_Buffer;
         Set_Msg_Quote;
      end if;
   end Set_Msg_Insertion_Reserved_Word;

   -------------------------------------
   -- Set_Msg_Insertion_Run_Time_Name --
   -------------------------------------

   procedure Set_Msg_Insertion_Run_Time_Name is
   begin
      if Targparm.Run_Time_Name_On_Target /= No_Name then
         Set_Msg_Blank_Conditional;
         Set_Msg_Char ('(');
         Get_Name_String (Targparm.Run_Time_Name_On_Target);
         Set_Casing (Mixed_Case);
         Set_Msg_Str (Name_Buffer (1 .. Name_Len));
         Set_Msg_Char (')');
      end if;
   end Set_Msg_Insertion_Run_Time_Name;

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

      --  The following assignment ensures that a second caret insertion
      --  character will correspond to the Error_Msg_Uint_2 parameter.

      Error_Msg_Uint_1 := Error_Msg_Uint_2;
   end Set_Msg_Insertion_Uint;

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
      Set_Msg_Str (Name_Buffer (1 .. Name_Len));
      Destroy_Global_Name_Buffer;
   end Set_Msg_Name_Buffer;

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
      --  Do replacement for special x'Class aspect names

      if Text = "_Pre" then
         Set_Msg_Str ("Pre'Class");

      elsif Text = "_Post" then
         Set_Msg_Str ("Post'Class");

      elsif Text = "_Type_Invariant" then
         Set_Msg_Str ("Type_Invariant'Class");

      elsif Text = "_pre" then
         Set_Msg_Str ("pre'class");

      elsif Text = "_post" then
         Set_Msg_Str ("post'class");

      elsif Text = "_type_invariant" then
         Set_Msg_Str ("type_invariant'class");

      elsif Text = "_PRE" then
         Set_Msg_Str ("PRE'CLASS");

      elsif Text = "_POST" then
         Set_Msg_Str ("POST'CLASS");

      elsif Text = "_TYPE_INVARIANT" then
         Set_Msg_Str ("TYPE_INVARIANT'CLASS");

      --  Preserve casing for names that include acronyms

      elsif Text = "Cpp_Class" then
         Set_Msg_Str ("CPP_Class");

      elsif Text = "Cpp_Constructor" then
         Set_Msg_Str ("CPP_Constructor");

      elsif Text = "Cpp_Virtual" then
         Set_Msg_Str ("CPP_Virtual");

      elsif Text = "Cpp_Vtable" then
         Set_Msg_Str ("CPP_Vtable");

      elsif Text = "Persistent_Bss" then
         Set_Msg_Str ("Persistent_BSS");

      elsif Text = "Spark_Mode" then
         Set_Msg_Str ("SPARK_Mode");

      elsif Text = "Use_Vads_Size" then
         Set_Msg_Str ("Use_VADS_Size");

      elsif Text = "Vads_Size" then
         Set_Msg_Str ("VADS_size");

      --  Normal case with no replacement

      else
         for J in Text'Range loop
            Set_Msg_Char (Text (J));
         end loop;
      end if;
   end Set_Msg_Str;

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

   ------------------------------
   -- Set_Specific_Warning_Off --
   ------------------------------

   procedure Set_Specific_Warning_Off
     (Loc    : Source_Ptr;
      Msg    : String;
      Reason : String_Id;
      Config : Boolean;
      Used   : Boolean := False)
   is
   begin
      Specific_Warnings.Append
        ((Start      => Loc,
          Msg        => new String'(Msg),
          Stop       => Source_Last (Get_Source_File_Index (Loc)),
          Reason     => Reason,
          Open       => True,
          Used       => Used,
          Config     => Config));
   end Set_Specific_Warning_Off;

   -----------------------------
   -- Set_Specific_Warning_On --
   -----------------------------

   procedure Set_Specific_Warning_On
     (Loc : Source_Ptr;
      Msg : String;
      Err : out Boolean)
   is
   begin
      for J in 1 .. Specific_Warnings.Last loop
         declare
            SWE : Specific_Warning_Entry renames Specific_Warnings.Table (J);

         begin
            if Msg = SWE.Msg.all
              and then Loc > SWE.Start
              and then SWE.Open
              and then Get_Source_File_Index (SWE.Start) =
                       Get_Source_File_Index (Loc)
            then
               SWE.Stop := Loc;
               SWE.Open := False;
               Err := False;

               --  If a config pragma is specifically cancelled, consider
               --  that it is no longer active as a configuration pragma.

               SWE.Config := False;
               return;
            end if;
         end;
      end loop;

      Err := True;
   end Set_Specific_Warning_On;

   ---------------------------
   -- Set_Warnings_Mode_Off --
   ---------------------------

   procedure Set_Warnings_Mode_Off (Loc : Source_Ptr; Reason : String_Id) is
   begin
      --  Don't bother with entries from instantiation copies, since we will
      --  already have a copy in the template, which is what matters.

      if Instantiation (Get_Source_File_Index (Loc)) /= No_Location then
         return;
      end if;

      --  If all warnings are suppressed by command line switch, this can
      --  be ignored, unless we are in GNATprove_Mode which requires pragma
      --  Warnings to be stored for the formal verification backend.

      if Warning_Mode = Suppress
        and then not GNATprove_Mode
      then
         return;
      end if;

      --  If last entry in table already covers us, this is a redundant pragma
      --  Warnings (Off) and can be ignored.

      if Warnings.Last >= Warnings.First
        and then Warnings.Table (Warnings.Last).Start <= Loc
        and then Loc <= Warnings.Table (Warnings.Last).Stop
      then
         return;
      end if;

      --  If none of those special conditions holds, establish a new entry,
      --  extending from the location of the pragma to the end of the current
      --  source file. This ending point will be adjusted by a subsequent
      --  corresponding pragma Warnings (On).

      Warnings.Append
        ((Start  => Loc,
          Stop   => Source_Last (Get_Source_File_Index (Loc)),
          Reason => Reason));
   end Set_Warnings_Mode_Off;

   --------------------------
   -- Set_Warnings_Mode_On --
   --------------------------

   procedure Set_Warnings_Mode_On (Loc : Source_Ptr) is
   begin
      --  Don't bother with entries from instantiation copies, since we will
      --  already have a copy in the template, which is what matters.

      if Instantiation (Get_Source_File_Index (Loc)) /= No_Location then
         return;
      end if;

      --  If all warnings are suppressed by command line switch, this can
      --  be ignored, unless we are in GNATprove_Mode which requires pragma
      --  Warnings to be stored for the formal verification backend.

      if Warning_Mode = Suppress
        and then not GNATprove_Mode
      then
         return;
      end if;

      --  If the last entry in the warnings table covers this pragma, then
      --  we adjust the end point appropriately.

      if Warnings.Last >= Warnings.First
        and then Warnings.Table (Warnings.Last).Start <= Loc
        and then Loc <= Warnings.Table (Warnings.Last).Stop
      then
         Warnings.Table (Warnings.Last).Stop := Loc;
      end if;
   end Set_Warnings_Mode_On;

   -------------------
   -- Sloc_In_Range --
   -------------------

   function Sloc_In_Range (Loc, Start, Stop : Source_Ptr) return Boolean is
      Cur_Loc : Source_Ptr := Loc;

   begin
      while Cur_Loc /= No_Location loop
         if Start <= Cur_Loc and then Cur_Loc <= Stop then
            return True;
         end if;

         Cur_Loc := Instantiation_Location (Cur_Loc);
      end loop;

      return False;
   end Sloc_In_Range;

   --------------------------------
   -- Validate_Specific_Warnings --
   --------------------------------

   procedure Validate_Specific_Warnings (Eproc : Error_Msg_Proc) is
   begin
      if not Warn_On_Warnings_Off then
         return;
      end if;

      for J in Specific_Warnings.First .. Specific_Warnings.Last loop
         declare
            SWE : Specific_Warning_Entry renames Specific_Warnings.Table (J);

         begin
            if not SWE.Config then

               --  Warn for unmatched Warnings (Off, ...)

               if SWE.Open then
                  Eproc.all
                    ("?.w?pragma Warnings Off with no matching Warnings On",
                     SWE.Start);

               --  Warn for ineffective Warnings (Off, ..)

               elsif not SWE.Used

                 --  Do not issue this warning for -Wxxx messages since the
                 --  back-end doesn't report the information. Note that there
                 --  is always an asterisk at the start of every message.

                 and then not
                   (SWE.Msg'Length > 3 and then SWE.Msg (2 .. 3) = "-W")
               then
                  Eproc.all
                    ("?.w?no warning suppressed by this pragma", SWE.Start);
               end if;
            end if;
         end;
      end loop;
   end Validate_Specific_Warnings;

   -------------------------------------
   -- Warning_Specifically_Suppressed --
   -------------------------------------

   function Warning_Specifically_Suppressed
     (Loc : Source_Ptr;
      Msg : String_Ptr;
      Tag : String := "") return String_Id
   is
   begin
      --  Loop through specific warning suppression entries

      for J in Specific_Warnings.First .. Specific_Warnings.Last loop
         declare
            SWE : Specific_Warning_Entry renames Specific_Warnings.Table (J);

         begin
            --  Pragma applies if it is a configuration pragma, or if the
            --  location is in range of a specific non-configuration pragma.

            if SWE.Config
              or else Sloc_In_Range (Loc, SWE.Start, SWE.Stop)
            then
               if Matches (Msg.all, SWE.Msg.all)
                 or else Matches (Tag, SWE.Msg.all)
               then
                  SWE.Used := True;
                  return SWE.Reason;
               end if;
            end if;
         end;
      end loop;

      return No_String;
   end Warning_Specifically_Suppressed;

   ------------------------------
   -- Warning_Treated_As_Error --
   ------------------------------

   function Warning_Treated_As_Error (Msg : String) return Boolean is
   begin
      for J in 1 .. Warnings_As_Errors_Count loop
         if Matches (Msg, Warnings_As_Errors (J).all) then
            return True;
         end if;
      end loop;

      return False;
   end Warning_Treated_As_Error;

   -------------------------
   -- Warnings_Suppressed --
   -------------------------

   function Warnings_Suppressed (Loc : Source_Ptr) return String_Id is
   begin
      --  Loop through table of ON/OFF warnings

      for J in Warnings.First .. Warnings.Last loop
         if Sloc_In_Range (Loc, Warnings.Table (J).Start,
                                Warnings.Table (J).Stop)
         then
            return Warnings.Table (J).Reason;
         end if;
      end loop;

      if Warning_Mode = Suppress then
         return Null_String_Id;
      else
         return No_String;
      end if;
   end Warnings_Suppressed;

end Erroutc;
