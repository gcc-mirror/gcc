------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               O U T P U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

package body Output is

   Buffer : String (1 .. Buffer_Max + 1) := (others => '*');
   for Buffer'Alignment use 4;
   --  Buffer used to build output line. We do line buffering because it is
   --  needed for the support of the debug-generated-code option (-gnatD). Note
   --  any attempt to write more output to a line than can fit in the buffer
   --  will be silently ignored. The alignment clause improves the efficiency
   --  of the save/restore procedures.

   Next_Col : Positive range 1 .. Buffer'Length + 1 := 1;
   --  Column about to be written

   Current_FD : File_Descriptor := Standout;
   --  File descriptor for current output

   type FD_Array is array (Nat range 1 .. 3) of File_Descriptor;
   FD_Stack     : FD_Array;
   FD_Stack_Idx : Nat := FD_Array'First - 1;
   --  Maintain a small stack for Push_Output and Pop_Output. We'd normally
   --  use Table for this and allow an unlimited depth, but we're the target
   --  of a pragma Elaborate_All in Table, so we can't use it here.

   Special_Output_Proc : Output_Proc := null;
   --  Record argument to last call to Set_Special_Output. If this is
   --  non-null, then we are in special output mode.

   Indentation_Amount : constant Positive := 3;
   --  Number of spaces to output for each indentation level

   Indentation_Limit : constant Positive := 40;
   --  Indentation beyond this number of spaces wraps around

   pragma Assert (Indentation_Limit < Buffer_Max / 2);
   --  Make sure this is substantially shorter than the line length

   Cur_Indentation : Natural := 0;
   --  Number of spaces to indent each line

   -----------------------
   -- Local_Subprograms --
   -----------------------

   procedure Flush_Buffer;
   --  Flush buffer if non-empty and reset column counter

   ---------------------------
   -- Cancel_Special_Output --
   ---------------------------

   procedure Cancel_Special_Output is
   begin
      Special_Output_Proc := null;
   end Cancel_Special_Output;

   ------------
   -- Column --
   ------------

   function Column return Pos is
   begin
      return Pos (Next_Col);
   end Column;

   ----------------------
   -- Delete_Last_Char --
   ----------------------

   procedure Delete_Last_Char is
   begin
      if Next_Col /= 1 then
         Next_Col := Next_Col - 1;
      end if;
   end Delete_Last_Char;

   ------------------
   -- Flush_Buffer --
   ------------------

   procedure Flush_Buffer is
      Write_Error : exception;
      --  Raised if Write fails

      ------------------
      -- Write_Buffer --
      ------------------

      procedure Write_Buffer (Buf : String);
      --  Write out Buf, either using Special_Output_Proc, or the normal way
      --  using Write. Raise Write_Error if Write fails (presumably due to disk
      --  full). Write_Error is not used in the case of Special_Output_Proc.

      procedure Write_Buffer (Buf : String) is
      begin
         --  If Special_Output_Proc has been set, then use it

         if Special_Output_Proc /= null then
            Special_Output_Proc.all (Buf);

         --  If output is not set, then output to either standard output
         --  or standard error.

         elsif Write (Current_FD, Buf'Address, Buf'Length) /= Buf'Length then
            raise Write_Error;

         end if;
      end Write_Buffer;

      Len : constant Natural := Next_Col - 1;

   --  Start of processing for Flush_Buffer

   begin
      if Len /= 0 then
         begin
            --  If there's no indentation, or if the line is too long with
            --  indentation, or if it's a blank line, just write the buffer.

            if Cur_Indentation = 0
              or else Cur_Indentation + Len > Buffer_Max
              or else Buffer (1 .. Len) = (1 => ASCII.LF)
            then
               Write_Buffer (Buffer (1 .. Len));

            --  Otherwise, construct a new buffer with preceding spaces, and
            --  write that.

            else
               declare
                  Indented_Buffer : constant String :=
                                      (1 .. Cur_Indentation => ' ') &
                                                          Buffer (1 .. Len);
               begin
                  Write_Buffer (Indented_Buffer);
               end;
            end if;

         exception
            when Write_Error =>

               --  If there are errors with standard error just quit. Otherwise
               --  set the output to standard error before reporting a failure
               --  and quitting.

               if Current_FD /= Standerr then
                  Current_FD := Standerr;
                  Next_Col := 1;
                  Write_Line ("fatal error: disk full");
               end if;

               OS_Exit (2);
         end;

         --  Buffer is now empty

         Next_Col := 1;
      end if;
   end Flush_Buffer;

   -------------------
   -- Ignore_Output --
   -------------------

   procedure Ignore_Output (S : String) is
   begin
      null;
   end Ignore_Output;

   ------------
   -- Indent --
   ------------

   procedure Indent is
   begin
      --  The "mod" in the following assignment is to cause a wrap around in
      --  the case where there is too much indentation.

      Cur_Indentation :=
        (Cur_Indentation + Indentation_Amount) mod Indentation_Limit;
   end Indent;

   ---------------
   -- Last_Char --
   ---------------

   function Last_Char return Character is
   begin
      if Next_Col /= 1 then
         return Buffer (Next_Col - 1);
      else
         return ASCII.NUL;
      end if;
   end Last_Char;

   -------------
   -- Outdent --
   -------------

   procedure Outdent is
   begin
      --  The "mod" here undoes the wrap around from Indent above

      Cur_Indentation :=
        (Cur_Indentation - Indentation_Amount) mod Indentation_Limit;
   end Outdent;

   ----------------
   -- Pop_Output --
   ----------------

   procedure Pop_Output is
   begin
      Flush_Buffer;
      pragma Assert (FD_Stack_Idx >= FD_Array'First);
      Current_FD := FD_Stack (FD_Stack_Idx);
      FD_Stack_Idx := FD_Stack_Idx - 1;
   end Pop_Output;

   -----------------
   -- Push_Output --
   -----------------

   procedure Push_Output is
   begin
      pragma Assert (FD_Stack_Idx < FD_Array'Last);
      FD_Stack_Idx := FD_Stack_Idx + 1;
      FD_Stack (FD_Stack_Idx) := Current_FD;
   end Push_Output;

   ---------------------------
   -- Restore_Output_Buffer --
   ---------------------------

   procedure Restore_Output_Buffer (S : Saved_Output_Buffer) is
   begin
      Next_Col := S.Next_Col;
      Cur_Indentation := S.Cur_Indentation;
      Buffer (1 .. Next_Col - 1) := S.Buffer (1 .. Next_Col - 1);
   end Restore_Output_Buffer;

   ------------------------
   -- Save_Output_Buffer --
   ------------------------

   function Save_Output_Buffer return Saved_Output_Buffer is
      S : Saved_Output_Buffer;
   begin
      S.Buffer (1 .. Next_Col - 1) := Buffer (1 .. Next_Col - 1);
      S.Next_Col := Next_Col;
      S.Cur_Indentation := Cur_Indentation;
      Next_Col := 1;
      Cur_Indentation := 0;
      return S;
   end Save_Output_Buffer;

   ------------------------
   -- Set_Special_Output --
   ------------------------

   procedure Set_Special_Output (P : Output_Proc) is
   begin
      Special_Output_Proc := P;
   end Set_Special_Output;

   ----------------
   -- Set_Output --
   ----------------

   procedure Set_Output (FD : File_Descriptor) is
   begin
      Flush_Buffer;
      Current_FD := FD;
   end Set_Output;

   ------------------------
   -- Set_Standard_Error --
   ------------------------

   procedure Set_Standard_Error is
   begin
      Set_Output (Standerr);
   end Set_Standard_Error;

   -------------------------
   -- Set_Standard_Output --
   -------------------------

   procedure Set_Standard_Output is
   begin
      Set_Output (Standout);
   end Set_Standard_Output;

   -------
   -- w --
   -------

   procedure w (C : Character) is
   begin
      Push_Output;
      Set_Standard_Error;

      Write_Char (''');
      Write_Char (C);
      Write_Char (''');
      Write_Eol;

      Pop_Output;
   end w;

   procedure w (S : String) is
   begin
      Push_Output;
      Set_Standard_Error;

      Write_Str (S);
      Write_Eol;

      Pop_Output;
   end w;

   procedure w (V : Int) is
   begin
      Push_Output;
      Set_Standard_Error;

      Write_Int (V);
      Write_Eol;

      Pop_Output;
   end w;

   procedure w (B : Boolean) is
   begin
      Push_Output;
      Set_Standard_Error;

      if B then
         w ("True");
      else
         w ("False");
      end if;

      Pop_Output;
   end w;

   procedure w (L : String; C : Character) is
   begin
      Push_Output;
      Set_Standard_Error;

      Write_Str (L);
      Write_Char (' ');
      w (C);

      Pop_Output;
   end w;

   procedure w (L : String; S : String) is
   begin
      Push_Output;
      Set_Standard_Error;

      Write_Str (L);
      Write_Char (' ');
      w (S);

      Pop_Output;
   end w;

   procedure w (L : String; V : Int) is
   begin
      Push_Output;
      Set_Standard_Error;

      Write_Str (L);
      Write_Char (' ');
      w (V);

      Pop_Output;
   end w;

   procedure w (L : String; B : Boolean) is
   begin
      Push_Output;
      Set_Standard_Error;

      Write_Str (L);
      Write_Char (' ');
      w (B);

      Pop_Output;
   end w;

   ----------------
   -- Write_Char --
   ----------------

   procedure Write_Char (C : Character) is
   begin
      if Next_Col > Buffer'Length then
         Flush_Buffer;
      end if;
      pragma Assert (Next_Col in Buffer'Range);

      if C = ASCII.LF then
         Write_Eol;
      else
         Buffer (Next_Col) := C;
         Next_Col := Next_Col + 1;
      end if;
   end Write_Char;

   ---------------
   -- Write_Eol --
   ---------------

   procedure Write_Eol is
   begin
      --  Remove any trailing spaces

      while Next_Col > 1 and then Buffer (Next_Col - 1) = ' ' loop
         Next_Col := Next_Col - 1;
      end loop;

      Buffer (Next_Col) := ASCII.LF;
      Next_Col := Next_Col + 1;
      Flush_Buffer;
   end Write_Eol;

   ---------------------------
   -- Write_Eol_Keep_Blanks --
   ---------------------------

   procedure Write_Eol_Keep_Blanks is
   begin
      Buffer (Next_Col) := ASCII.LF;
      Next_Col := Next_Col + 1;
      Flush_Buffer;
   end Write_Eol_Keep_Blanks;

   ----------------------
   -- Write_Erase_Char --
   ----------------------

   procedure Write_Erase_Char (C : Character) is
   begin
      if Next_Col /= 1 and then Buffer (Next_Col - 1) = C then
         Next_Col := Next_Col - 1;
      end if;
   end Write_Erase_Char;

   ---------------
   -- Write_Int --
   ---------------

   procedure Write_Int (Val : Int) is
      --  Type Int has one extra negative number (i.e. two's complement), so we
      --  work with negative numbers here. Otherwise, negating Int'First will
      --  overflow.

      subtype Nonpositive is Int range Int'First .. 0;
      procedure Write_Abs (Val : Nonpositive);
      --  Write out the absolute value of Val

      procedure Write_Abs (Val : Nonpositive) is
      begin
         if Val < -9 then
            Write_Abs (Val / 10); -- Recursively write higher digits
         end if;

         Write_Char (Character'Val (-(Val rem 10) + Character'Pos ('0')));
      end Write_Abs;

   begin
      if Val < 0 then
         Write_Char ('-');
         Write_Abs (Val);
      else
         Write_Abs (-Val);
      end if;
   end Write_Int;

   ------------------
   -- Write_Int_64 --
   ------------------

   procedure Write_Int_64 (Val : Int_64) is
      subtype Nonpositive is Int_64 range Int_64'First .. 0;
      procedure Write_Abs (Val : Nonpositive);

      procedure Write_Abs (Val : Nonpositive) is
      begin
         if Val < -9 then
            Write_Abs (Val / 10);
         end if;

         Write_Char (Character'Val (-(Val rem 10) + Character'Pos ('0')));
      end Write_Abs;

   begin
      if Val < 0 then
         Write_Char ('-');
         Write_Abs (Val);
      else
         Write_Abs (-Val);
      end if;
   end Write_Int_64;

   ----------------
   -- Write_Line --
   ----------------

   procedure Write_Line (S : String) is
   begin
      Write_Str (S);
      Write_Eol;
   end Write_Line;

   ------------------
   -- Write_Spaces --
   ------------------

   procedure Write_Spaces (N : Nat) is
   begin
      for J in 1 .. N loop
         Write_Char (' ');
      end loop;
   end Write_Spaces;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str (S : String) is
   begin
      for J in S'Range loop
         Write_Char (S (J));
      end loop;
   end Write_Str;

end Output;
