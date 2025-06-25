------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E R R O U T C                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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
with Sinfo.Nodes;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stylesw;  use Stylesw;
with Targparm;
with Uintp;    use Uintp;
with Widechar; use Widechar;
with Warnsw;   use Warnsw;

package body Erroutc is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Sloc_In_Range (Loc, Start, Stop : Source_Ptr) return Boolean;
   --  Return whether Loc is in the range Start .. Stop, taking instantiation
   --  locations of Loc into account. This is useful for suppressing warnings
   --  from generic instantiations by using pragma Warnings around generic
   --  instances, as needed in GNATprove.

   function Has_Switch_Tag (Id : Error_Msg_Id) return Boolean;
   function Has_Switch_Tag (E_Msg : Error_Msg_Object) return Boolean;
   --  Returns True if the E_Msg is Warning, Style or Info and has a non-empty
   --  Warn_Char.

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

            Decrease_Error_Msg_Count (Errors.Table (D));

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
      Warnings_Count : constant Int := Warnings_Detected;
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
            if Errors.Table (J).Kind = Warning
               and then Errors.Table (J).Compile_Time_Pragma
               and then not Errors.Table (J).Deleted
            then
               Result := Result + 1;
            end if;
         end;
      end loop;
      return Result;
   end Count_Compile_Time_Pragma_Warnings;

   ------------------------------
   -- Decrease_Error_Msg_Count --
   ------------------------------

   procedure Decrease_Error_Msg_Count (E : Error_Msg_Object) is

   begin
      case E.Kind is
         when Info =>
            Info_Messages := Info_Messages - 1;

         when Warning | Style =>
            Warnings_Detected := Warnings_Detected - 1;

         when High_Check | Medium_Check | Low_Check =>
            Check_Messages := Check_Messages - 1;

         when Error =>
            Total_Errors_Detected := Total_Errors_Detected - 1;
            Serious_Errors_Detected := Serious_Errors_Detected - 1;

         when Non_Serious_Error =>
            Total_Errors_Detected := Total_Errors_Detected - 1;
      end case;
   end Decrease_Error_Msg_Count;

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
      w ("  Text               = ", E.Text.all);
      w ("  Next               = ", Int (E.Next));
      w ("  Prev               = ", Int (E.Prev));
      w ("  Sfile              = ", Int (E.Sfile));

      Write_Str
        ("  Sptr               = ");
      Write_Location (E.Sptr.Ptr);  --  ??? Do not write the full span for now
      Write_Eol;

      Write_Str
        ("  Optr               = ");
      Write_Location (E.Optr.Ptr);
      Write_Eol;

      Write_Str
        ("  Insertion_Sloc     = ");
      Write_Location (E.Insertion_Sloc);
      Write_Eol;

      w ("  Line               = ", Int (E.Line));
      w ("  Col                = ", Int (E.Col));
      w ("  Kind               = ", E.Kind'Img);
      w ("  Warn_Err           = ", E.Warn_Err);
      w ("  Warn_Chr           = '" & E.Warn_Chr & ''');
      w ("  Uncond             = ", E.Uncond);
      w ("  Msg_Cont           = ", E.Msg_Cont);
      w ("  Deleted            = ", E.Deleted);

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
      Is_Style : constant Boolean         := Errors.Table (Id).Kind in Style;
      Warn_Chr : constant String (1 .. 2) := Errors.Table (Id).Warn_Chr;

   begin
      if Has_Switch_Tag (Errors.Table (Id))
        and then Warn_Chr (1) /= '?'
      then
         if Warn_Chr = "$ " then
            return "-gnatel";
         elsif Is_Style then
            return "-gnaty" & Warn_Chr (1);
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
      Warn_Chr : constant String (1 .. 2) := Errors.Table (Id).Warn_Chr;
      Option   : constant String          := Get_Warning_Option (Id);

   begin
      if Has_Switch_Tag (Id) then
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

   ------------------------------
   -- Increase_Error_Msg_Count --
   ------------------------------

   procedure Increase_Error_Msg_Count (E : Error_Msg_Object) is

   begin
      case E.Kind is
         when Info =>
            Info_Messages := Info_Messages + 1;

         when Warning | Style =>
            Warnings_Detected := Warnings_Detected + 1;

         when High_Check | Medium_Check | Low_Check =>
            Check_Messages := Check_Messages + 1;

         when Error =>
            Total_Errors_Detected := Total_Errors_Detected + 1;
            Serious_Errors_Detected := Serious_Errors_Detected + 1;

         when Non_Serious_Error =>
            Total_Errors_Detected := Total_Errors_Detected + 1;
      end case;
   end Increase_Error_Msg_Count;

   --------------------------------
   -- Is_Redundant_Error_Message --
   --------------------------------

   function Is_Redundant_Error_Message
     (Prev_Msg : Error_Msg_Id; Cur_Msg : Error_Msg_Id) return Boolean is

   begin
      return
        Prev_Msg /= No_Error_Msg

        --  Error messages are posted on the same line

        and then Errors.Table (Prev_Msg).Line = Errors.Table (Cur_Msg).Line
        and then Errors.Table (Prev_Msg).Sfile = Errors.Table (Cur_Msg).Sfile

        --  Do not consider unconditional messages to be redundant right now
        --  They may be removed later.

        and then not Errors.Table (Cur_Msg).Uncond

        --  Do not consider continuation messages as they are removed with
        --  their parent later on.

        and then not Errors.Table (Cur_Msg).Msg_Cont

        --  Don't delete if prev msg is warning and new msg is an error.
        --  This is because we don't want a real error masked by a
        --  warning. In all other cases (that is parse errors for the
        --  same line that are not unconditional) we do delete the
        --  message. This helps to avoid junk extra messages from
        --  cascaded parsing errors

        and then (Errors.Table (Prev_Msg).Kind not in Warning | Style
                  or else Errors.Table (Cur_Msg).Kind in Warning | Style);
   end Is_Redundant_Error_Message;

   --------------------
   -- Has_Switch_Tag --
   --------------------

   function Has_Switch_Tag (Id : Error_Msg_Id) return Boolean
   is (Has_Switch_Tag (Errors.Table (Id)));

   function Has_Switch_Tag (E_Msg : Error_Msg_Object) return Boolean is
   begin
      return
        E_Msg.Kind in Warning | Info | Style and then E_Msg.Warn_Chr /= "  ";
   end Has_Switch_Tag;

   --------------------
   -- Next_Error_Msg --
   --------------------

   procedure Next_Error_Msg (E : in out Error_Msg_Id) is
   begin
      loop
         E := Errors.Table (E).Next;
         exit when E = No_Error_Msg;
         exit when not Errors.Table (E).Deleted
           and then not Errors.Table (E).Msg_Cont;
      end loop;
   end Next_Error_Msg;

   ---------------------------
   -- Next_Continuation_Msg --
   ---------------------------

   procedure Next_Continuation_Msg (E : in out Error_Msg_Id) is
   begin
      E := Errors.Table (E).Next;

      if E = No_Error_Msg or else not Errors.Table (E).Msg_Cont then
         E := No_Error_Msg;
      end if;
   end Next_Continuation_Msg;

   ----------------------
   -- Primary_Location --
   ----------------------

   function Primary_Location (E : Error_Msg_Object) return Labeled_Span_Id is
      L : Labeled_Span_Id;
   begin
      L := E.Locations;
      while L /= No_Labeled_Span loop
         if Locations.Table (L).Is_Primary then
            return L;
         end if;

         L := Locations.Table (L).Next;
      end loop;

      return No_Labeled_Span;
   end Primary_Location;

   ------------------
   -- Get_Human_Id --
   ------------------

   function Get_Human_Id (E : Error_Msg_Object) return String_Ptr is
   begin
      if E.Switch = No_Switch_Id then
         return Diagnostic_Entries (E.Id).Human_Id;
      else
         return Get_Switch (E).Human_Id;
      end if;
   end Get_Human_Id;

   --------------------
   -- Get_Doc_Switch --
   --------------------

   function Get_Doc_Switch (E : Error_Msg_Object) return String is
   begin
      if Warning_Doc_Switch
        and then E.Warn_Chr /= "  "
        and then E.Kind in Info
          | Style
          | Warning
      then
         if E.Switch = No_Switch_Id then
            if E.Warn_Chr = "* " then
               return "[restriction warning]";

               --  Info messages can have a switch tag but they should not have
               --  a default switch tag.

            elsif E.Kind /= Info then

               --  For Default_Warning

               return "[enabled by default]";
            end if;
         else
            declare
               S : constant Switch_Type := Get_Switch (E);
            begin
               return "[-" & S.Short_Name.all & "]";
            end;
         end if;
      end if;

      return "";
   end Get_Doc_Switch;

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (E : Error_Msg_Object) return Switch_Type is
   begin
      return Get_Switch (E.Switch);
   end Get_Switch;

   -------------------
   -- Get_Switch_Id --
   -------------------

   function Get_Switch_Id (E : Error_Msg_Object) return Switch_Id is
   begin
      return Get_Switch_Id (E.Kind, E.Warn_Chr);
   end Get_Switch_Id;

   function Get_Switch_Id
     (Kind : Error_Msg_Type; Warn_Chr : String) return Switch_Id is
   begin
      if Warn_Chr = "$ " then
         return Get_Switch_Id ("gnatel");
      elsif Kind in Warning | Info then
         return Get_Switch_Id ("gnatw" & Warn_Chr);
      elsif Kind = Style then
         return Get_Switch_Id ("gnaty" & Warn_Chr);
      else
         return No_Switch_Id;
      end if;
   end Get_Switch_Id;

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

   ------------------------
   -- Output_Text_Within --
   ------------------------

   procedure Output_Text_Within (Txt : String; Line_Length : Nat) is
      Offs : constant Nat := Column - 1;
      --  Offset to start of message, used for continuations

      Ptr   : Natural;

      Split : Natural;
      --   Position where a new line was inserted in the original message

      Start : Natural;
      --   Start of the current line

      Max   : Integer := Integer (Line_Length - Column + 1);
      --  Maximum characters to output on next line

      Text_Length : constant Natural := Txt'Length;
      --  Length of the message

   begin
      --  Here we have to split the message up into multiple lines

      Ptr := 1;
      loop
         --  Make sure we do not have ludicrously small line

         Max := Integer'Max (Max, 20);

         --  If remaining text fits, output it respecting LF and we are done

         if Text_Length - Ptr < Max then
            for J in Ptr .. Text_Length loop
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

         Max := Integer (Line_Length - Column + 1);
      end loop;
   end Output_Text_Within;

   -------------------------
   -- Output_Msg_Location --
   -------------------------

   procedure Output_Msg_Location (E : Error_Msg_Id) is
      E_Obj : constant Error_Msg_Object := Errors.Table (E);
   begin
      Write_Str (SGR_Locus);

      if Full_Path_Name_For_Brief_Errors then
         Write_Name (Full_Ref_Name (E_Obj.Sfile));
      else
         Write_Name (Reference_Name (E_Obj.Sfile));
      end if;

      Write_Char (':');
      Write_Int (Int (Physical_To_Logical (E_Obj.Line, E_Obj.Sfile)));
      Write_Char (':');

      if E_Obj.Col < 10 then
         Write_Char ('0');
      end if;

      Write_Int (Int (E_Obj.Col));
      Write_Str (": ");

      Write_Str (SGR_Reset);
   end Output_Msg_Location;

   ---------------------
   -- Output_Msg_Text --
   ---------------------

   procedure Output_Msg_Text (E : Error_Msg_Id) is

      E_Msg       : Error_Msg_Object renames Errors.Table (E);
      Text        : constant String_Ptr := E_Msg.Text;
      Tag         : constant String     := Get_Warning_Tag (E);
      SGR_Code    : constant String     := Get_SGR_Code (E_Msg);
      Kind_Prefix : constant String     :=
        (if E_Msg.Kind = Style then Style_Prefix
         else Kind_To_String (E_Msg) & ": ");
      Buf         : Bounded_String (Max_Msg_Length);
      Line_Length : constant Nat        :=
        (if Error_Msg_Line_Length = 0 then Nat'Last
         else Error_Msg_Line_Length);

   begin
      --  Prefix with "error:" rather than warning.
      --  Additionally include the style suffix when needed.

      if E_Msg.Warn_Err then

         Warnings_Treated_As_Errors := Warnings_Treated_As_Errors + 1;

         Append
           (Buf,
            SGR_Error & "error: " & SGR_Reset &
            (if E_Msg.Kind = Style then Style_Prefix else ""));

      --  Print the message kind prefix
      --  * Info/Style/Warning messages
      --  * Check messages that are not continuations in the pretty printer
      --  * Error messages when error tags are allowed

      elsif E_Msg.Kind in Info | Style | Warning
        or else
        (E_Msg.Kind in High_Check | Medium_Check | Low_Check
         and then not (E_Msg.Msg_Cont and then Debug_Flag_FF))
        or else
        (E_Msg.Kind in Error | Non_Serious_Error
         and then Opt.Unique_Error_Tag)
      then
         Append (Buf, SGR_Code & Kind_Prefix & SGR_Reset);
      end if;

      Append (Buf, Text.all);

      --  Postfix warning tag to message if needed

      if Tag /= "" and then Warning_Doc_Switch then
         Append (Buf, ' ' & Tag);
      end if;

      --  Postfix [warning-as-error] at the end

      if E_Msg.Warn_Err then
         Append (Buf, " [warning-as-error]");
      end if;

      Output_Text_Within (To_String (Buf), Line_Length);
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
           and then Msg (J) in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '*' | '$'
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
      --  Continuation lines need to check only for insertion sequences.
      --  Other attributes should be inherited from the main message.

      if Msg (Msg'First) = '\' then
         Has_Insertion_Line := False;

         J := Msg'First;

         --  If we have a quote, don't look at following character

         while J <= Msg'Last loop
            if Msg (J) = ''' then
               J := J + 2;

            --  Insertion line (# insertion)

            elsif Msg (J) = '#' then
               Has_Insertion_Line := True;
               J := J + 1;
            else
               J := J + 1;
            end if;
         end loop;

         return;

      --  Some global variables are not set for continuation messages, as they
      --  only make sense for the initial message.

      else

         --  Set initial values of globals (may be changed during scan)

         Error_Msg_Kind       := Error;
         Is_Unconditional_Msg := False;
         Is_Runtime_Raise     := False;
         Warning_Msg_Char     := "  ";

         --  Check style message

         if Msg'Length > Style_Prefix'Length
           and then
             Msg (Msg'First .. Msg'First + Style_Prefix'Length - 1) =
             Style_Prefix
         then
            Error_Msg_Kind := Style;

            --  Check info message

         elsif Msg'Length > Info_Prefix'Length
           and then
             Msg (Msg'First .. Msg'First + Info_Prefix'Length - 1) =
             Info_Prefix
         then
            Error_Msg_Kind := Info;

            --  Check high check message

         elsif Msg'Length > High_Prefix'Length
           and then
             Msg (Msg'First .. Msg'First + High_Prefix'Length - 1) =
             High_Prefix
         then
            Error_Msg_Kind := High_Check;

            --  Check medium check message

         elsif Msg'Length > Medium_Prefix'Length
           and then
             Msg (Msg'First .. Msg'First + Medium_Prefix'Length - 1) =
             Medium_Prefix
         then
            Error_Msg_Kind := Medium_Check;

            --  Check low check message

         elsif Msg'Length > Low_Prefix'Length
           and then
             Msg (Msg'First .. Msg'First + Low_Prefix'Length - 1) =
             Low_Prefix
         then
            Error_Msg_Kind := Low_Check;
         end if;
      end if;

      Has_Double_Exclam  := False;
      Has_Error_Code     := False;
      Has_Insertion_Line := False;

      --  Loop through message looking for relevant insertion sequences

      J := Msg'First;
      while J <= Msg'Last loop

         --  If we have a quote, don't look at following character

         if Msg (J) = ''' then
            J := J + 2;

         --  Warning message (? or < insertion sequence)

         elsif Msg (J) = '?' or else Msg (J) = '<' then
            if Msg (J) = '?' or else Error_Msg_Warn then

               --  Consider Info and Style messages as unique message types.
               --  Those messages can have warning insertion characters within
               --  them. However they should only be switch specific insertion
               --  characters and not the generic ? or ?? warning insertion
               --  characters.

               if Error_Msg_Kind not in Style | Info then
                  Error_Msg_Kind := Warning;
               end if;

               J := J + 1;
               Warning_Msg_Char := Parse_Message_Class;

               --  Bomb if untagged warning message. This code can be
               --  uncommented for debugging when looking for untagged warning
               --  messages.

               --  pragma Assert (Warning_Msg_Char /= "  ");

            else
               J := J + 1;
            end if;

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
            Error_Msg_Kind := Non_Serious_Error;
            J := J + 1;

         --  Error code ([] insertion)

         elsif Msg (J) = '['
           and then J < Msg'Last
           and then Msg (J + 1) = ']'
         then
            Has_Error_Code := True;
            J := J + 2;

         else
            J := J + 1;
         end if;
      end loop;
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
            Decrease_Error_Msg_Count (Errors.Table (E));

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

   ----------------------------
   -- Set_Msg_Insertion_Code --
   ----------------------------

   procedure Set_Msg_Insertion_Code is
      H : constant array (Nat range 0 .. 9) of Character := "0123456789";
      P10 : constant array (Natural range 0 .. 3) of Nat :=
        (10 ** 0,
         10 ** 1,
         10 ** 2,
         10 ** 3);

      Code_Len : constant Natural :=
        (case Error_Msg_Code is
           when    0         => 0,
           when    1 ..    9 => 1,
           when   10 ..   99 => 2,
           when  100 ..  999 => 3,
           when 1000 .. 9999 => 4);
      Code_Rest  : Nat := Error_Msg_Code;
      Code_Digit : Nat;

   begin
      Set_Msg_Char ('E');

      for J in 1 .. Error_Msg_Code_Digits - Code_Len loop
         Set_Msg_Char ('0');
      end loop;

      for J in 1 .. Code_Len loop
         Code_Digit := Code_Rest / P10 (Code_Len - J);
         Set_Msg_Char (H (Code_Digit));
         Code_Rest := Code_Rest - Code_Digit * P10 (Code_Len - J);
      end loop;
   end Set_Msg_Insertion_Code;

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

      --  We make a similar exception for CUDA

      elsif Name_Len = 4 and then Name_Buffer (1 .. 4) = "CUDA" then
         Set_Msg_Name_Buffer;

      --  We make a similar exception for SPARK

      elsif Name_Len = 5 and then Name_Buffer (1 .. 5) = "SPARK" then
         Set_Msg_Name_Buffer;

      --  Otherwise, case appropriately and add surrounding quotes

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

      elsif Text = "Cuda_Device" then
         Set_Msg_Str ("CUDA_Device");

      elsif Text = "Cuda_Global" then
         Set_Msg_Str ("CUDA_Global");

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
     (Node   : Node_Id;
      Msg    : String;
      Reason : String_Id;
      Config : Boolean;
      Used   : Boolean := False)
   is
      Loc : constant Source_Ptr := Sinfo.Nodes.Sloc (Node);
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

   -------------------------
   -- Write_Error_Summary --
   -------------------------

   procedure Write_Error_Summary is
   begin
      --  Extra blank line if error messages or source listing were output

      if Total_Errors_Detected + Warnings_Detected > 0 or else Full_List
      then
         Write_Eol;
      end if;

      --  Message giving number of lines read and number of errors detected.
      --  This normally goes to Standard_Output. The exception is when brief
      --  mode is not set, verbose mode (or full list mode) is set, and
      --  there are errors. In this case we send the message to standard
      --  error to make sure that *something* appears on standard error
      --  in an error situation.

      if Total_Errors_Detected + Warnings_Detected /= 0
         and then not Brief_Output
         and then (Verbose_Mode or Full_List)
      then
         Set_Standard_Error;
      end if;

      --  Message giving total number of lines. Don't give this message if
      --  the Main_Source line is unknown (this happens in error situations,
      --  e.g. when integrated preprocessing fails).

      if Main_Source_File > No_Source_File then
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

      --  We now need to output warnings. When using -gnatwe, all warnings
      --  should be treated as errors, except for warnings originating from
      --  the use of the Compile_Time_Warning pragma. Another situation
      --  where a warning might be treated as an error is when the source
      --  code contains a Warning_As_Error pragma.
      --  When warnings are treated as errors, we still log them as
      --  warnings, but we add a message denoting how many of these warnings
      --  are also errors.

      declare
         Warnings_Count : constant Int := Warnings_Detected;

         Compile_Time_Warnings : Int;
         --  Number of warnings that come from a Compile_Time_Warning
         --  pragma.

         Non_Compile_Time_Warnings : Int;
         --  Number of warnings that do not come from a Compile_Time_Warning
         --  pragmas.

      begin
         if Warnings_Count > 0 then
            Write_Str (", ");
            Write_Int (Warnings_Count);
            Write_Str (" warning");

            if Warnings_Count > 1 then
               Write_Char ('s');
            end if;

            Compile_Time_Warnings := Count_Compile_Time_Pragma_Warnings;
            Non_Compile_Time_Warnings :=
               Warnings_Count - Compile_Time_Warnings;

            if Warning_Mode = Treat_As_Error
               and then Non_Compile_Time_Warnings > 0
            then
               Write_Str (" (");

               if Compile_Time_Warnings > 0 then
                  Write_Int (Non_Compile_Time_Warnings);
                  Write_Str (" ");
               end if;

               Write_Str ("treated as error");

               if Non_Compile_Time_Warnings > 1 then
                  Write_Char ('s');
               end if;

               Write_Char (')');

            elsif Warnings_Treated_As_Errors > 0 then
               Write_Str (" (");

               if Warnings_Treated_As_Errors /= Warnings_Count then
                  Write_Int (Warnings_Treated_As_Errors);
                  Write_Str (" ");
               end if;

               Write_Str ("treated as error");

               if Warnings_Treated_As_Errors > 1 then
                  Write_Str ("s");
               end if;

               Write_Str (")");
            end if;
         end if;
      end;

      if Info_Messages /= 0 then
         Write_Str (", ");
         Write_Int (Info_Messages);
         Write_Str (" info message");

         if Info_Messages > 1 then
            Write_Char ('s');
         end if;
      end if;

      Write_Eol;
      Set_Standard_Output;
   end Write_Error_Summary;

end Erroutc;
