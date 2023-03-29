------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        G N A T . P E R F E C T _ H A S H _ G E N E R A T O R S           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2023, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.IO_Exceptions;       use Ada.IO_Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNAT.OS_Lib;             use GNAT.OS_Lib;

package body GNAT.Perfect_Hash_Generators is

   use SPHG;

   function Image (Int : Integer; W : Natural := 0) return String;
   function Image (Str : String;  W : Natural := 0) return String;
   --  Return a string which includes string Str or integer Int preceded by
   --  leading spaces if required by width W.

   EOL : constant Character := ASCII.LF;

   Max  : constant := 78;
   Last : Natural  := 0;
   Line : String (1 .. Max);
   --  Use this line to provide buffered IO

   NK : Natural  := 0;
   --  NK : Number of Keys

   Opt : Optimization;
   --  Optimization mode (memory vs CPU)

   procedure Add (C : Character);
   procedure Add (S : String);
   --  Add a character or a string in Line and update Last

   procedure Put
     (F  : File_Descriptor;
      S  : String;
      F1 : Natural;
      L1 : Natural;
      C1 : Natural;
      F2 : Natural;
      L2 : Natural;
      C2 : Natural);
   --  Write string S into file F as a element of an array of one or two
   --  dimensions. Fk (resp. Lk and Ck) indicates the first (resp last and
   --  current) index in the k-th dimension. If F1 = L1 the array is considered
   --  as a one dimension array. This dimension is described by F2 and L2. This
   --  routine takes care of all the parenthesis, spaces and commas needed to
   --  format correctly the array. Moreover, the array is well indented and is
   --  wrapped to fit in a 80 col line. When the line is full, the routine
   --  writes it into file F. When the array is completed, the routine adds
   --  semi-colon and writes the line into file F.

   procedure New_Line (File : File_Descriptor);
   --  Simulate Ada.Text_IO.New_Line with GNAT.OS_Lib

   procedure Put (File : File_Descriptor; Str : String);
   --  Simulate Ada.Text_IO.Put with GNAT.OS_Lib

   procedure Put_Int_Matrix
     (File  : File_Descriptor;
      Title : String;
      Table : Table_Name;
      Len_1 : Natural;
      Len_2 : Natural);
   --  Output a title and a matrix. When the matrix has only one non-empty
   --  dimension (Len_2 = 0), output a vector.

   function Ada_File_Base_Name (Pkg_Name : String) return String;
   --  Return the base file name (i.e. without .ads/.adb extension) for an
   --  Ada source file containing the named package, using the standard GNAT
   --  file-naming convention. For example, if Pkg_Name is "Parent.Child", we
   --  return "parent-child".

   ------------------------
   -- Ada_File_Base_Name --
   ------------------------

   function Ada_File_Base_Name (Pkg_Name : String) return String is
   begin
      --  Convert to lower case, then replace '.' with '-'

      return Result : String := To_Lower (Pkg_Name) do
         for J in Result'Range loop
            if Result (J) = '.' then
               Result (J) := '-';
            end if;
         end loop;
      end return;
   end Ada_File_Base_Name;

   ---------
   -- Add --
   ---------

   procedure Add (C : Character) is
      pragma Assert (C /= ASCII.NUL);
   begin
      Line (Last + 1) := C;
      Last := Last + 1;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (S : String) is
      Len : constant Natural := S'Length;
   begin
      for J in S'Range loop
         pragma Assert (S (J) /= ASCII.NUL);
         null;
      end loop;

      Line (Last + 1 .. Last + Len) := S;
      Last := Last + Len;
   end Add;

   -------------
   -- Compute --
   -------------

   procedure Compute (Position : String := Default_Position) is
   begin
      SPHG.Compute (Position);
   end Compute;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      NK := 0;
      SPHG.Finalize;
   end Finalize;

   -----------
   -- Image --
   -----------

   function Image (Int : Integer; W : Natural := 0) return String is
      B : String (1 .. 32);
      L : Natural := 0;

      procedure Img (V : Natural);
      --  Compute image of V into B, starting at B (L), incrementing L

      ---------
      -- Img --
      ---------

      procedure Img (V : Natural) is
      begin
         if V > 9 then
            Img (V / 10);
         end if;

         L := L + 1;
         B (L) := Character'Val ((V mod 10) + Character'Pos ('0'));
      end Img;

   --  Start of processing for Image

   begin
      if Int < 0 then
         L := L + 1;
         B (L) := '-';
         Img (-Int);
      else
         Img (Int);
      end if;

      return Image (B (1 .. L), W);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Str : String; W : Natural := 0) return String is
      Len : constant Natural := Str'Length;
      Max : Natural := Len;

   begin
      if Max < W then
         Max := W;
      end if;

      declare
         Buf : String (1 .. Max) := (1 .. Max => ' ');

      begin
         for J in 0 .. Len - 1 loop
            Buf (Max - Len + 1 + J) := Str (Str'First + J);
         end loop;

         return Buf;
      end;
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Seed   : Natural;
      K_To_V : Float        := Default_K_To_V;
      Optim  : Optimization := Memory_Space;
      Tries  : Positive     := Default_Tries)
   is
      V : constant Positive := Positive (Float (NK) * K_To_V);

   begin
      Opt := Optim;
      SPHG.Initialize (Seed, V, SPHG.Optimization (Optim), Tries);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert (Value : String) is
   begin
      NK := NK + 1;
      SPHG.Insert (Value);
   end Insert;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (File : File_Descriptor) is
   begin
      if Write (File, EOL'Address, 1) /= 1 then
         raise Program_Error;
      end if;
   end New_Line;

   -------------
   -- Produce --
   -------------

   procedure Produce
     (Pkg_Name   : String  := Default_Pkg_Name;
      Use_Stdout : Boolean := False)
   is
      File : File_Descriptor := Standout;

      Siz, L1, L2 : Natural;
      --  For calls to Define

      Status : Boolean;
      --  For call to Close

      function Array_Img (N, T, R1 : String; R2 : String := "") return String;
      --  Return string "N : constant array (R1[, R2]) of T;"

      function Range_Img (F, L : Natural; T : String := "") return String;
      --  Return string "[T range ]F .. L"

      function Type_Img (Siz : Positive) return String;
      --  Return the name of the unsigned type of size S

      ---------------
      -- Array_Img --
      ---------------

      function Array_Img
        (N, T, R1 : String;
         R2       : String := "") return String
      is
      begin
         Last := 0;
         Add ("   ");
         Add (N);
         Add (" : constant array (");
         Add (R1);

         if R2 /= "" then
            Add (", ");
            Add (R2);
         end if;

         Add (") of ");
         Add (T);
         Add (" :=");
         return Line (1 .. Last);
      end Array_Img;

      ---------------
      -- Range_Img --
      ---------------

      function Range_Img (F, L : Natural; T : String := "") return String is
         FI  : constant String  := Image (F);
         FL  : constant Natural := FI'Length;
         LI  : constant String  := Image (L);
         LL  : constant Natural := LI'Length;
         TL  : constant Natural := T'Length;
         RI  : String (1 .. TL + 7 + FL + 4 + LL);
         Len : Natural := 0;

      begin
         if TL /= 0 then
            RI (Len + 1 .. Len + TL) := T;
            Len := Len + TL;
            RI (Len + 1 .. Len + 7) := " range ";
            Len := Len + 7;
         end if;

         RI (Len + 1 .. Len + FL) := FI;
         Len := Len + FL;
         RI (Len + 1 .. Len + 4) := " .. ";
         Len := Len + 4;
         RI (Len + 1 .. Len + LL) := LI;
         Len := Len + LL;
         return RI (1 .. Len);
      end Range_Img;

      --------------
      -- Type_Img --
      --------------

      function Type_Img (Siz : Positive) return String is
         S : constant String := Image (Siz);
         U : String  := "Unsigned_  ";
         N : Natural := 9;

      begin
         for J in S'Range loop
            N := N + 1;
            U (N) := S (J);
         end loop;

         return U (1 .. N);
      end Type_Img;

      P : Natural;

      FName : String := Ada_File_Base_Name (Pkg_Name) & ".ads";
      --  Initially, the name of the spec file, then modified to be the name of
      --  the body file. Not used if Use_Stdout is True.

   --  Start of processing for Produce

   begin
      if not Use_Stdout then
         File := Create_File (FName, Binary);

         if File = Invalid_FD then
            raise Program_Error with "cannot create: " & FName;
         end if;
      end if;

      Put      (File, "package ");
      Put      (File, Pkg_Name);
      Put      (File, " is");
      New_Line (File);
      Put      (File, "   function Hash (S : String) return Natural;");
      New_Line (File);
      Put      (File, "end ");
      Put      (File, Pkg_Name);
      Put      (File, ";");
      New_Line (File);

      if not Use_Stdout then
         Close (File, Status);

         if not Status then
            raise Device_Error;
         end if;
      end if;

      if not Use_Stdout then

         --  Set to body file name

         FName (FName'Last) := 'b';

         File := Create_File (FName, Binary);

         if File = Invalid_FD then
            raise Program_Error with "cannot create: " & FName;
         end if;
      end if;

      Put      (File, "with Interfaces; use Interfaces;");
      New_Line (File);
      New_Line (File);
      Put      (File, "package body ");
      Put      (File, Pkg_Name);
      Put      (File, " is");
      New_Line (File);
      New_Line (File);

      if Opt = CPU_Time then
         --  The format of this table is fixed

         Define (Used_Character_Set, Siz, L1, L2);
         pragma Assert (L1 = 256 and then L2 = 0);

         Put      (File, Array_Img ("C", Type_Img (Siz), "Character"));
         New_Line (File);

         for J in 0 .. 255 loop
            P := Value (Used_Character_Set, J);
            Put (File, Image (P), 1, 0, 1, 0, 255, J);
         end loop;

         New_Line (File);
      end if;

      Define (Character_Position, Siz, L1, L2);
      pragma Assert (Siz = 31 and then L2 = 0);

      Put      (File, Array_Img ("P", "Natural", Range_Img (0, L1 - 1)));
      New_Line (File);

      for J in 0 .. L1 - 1 loop
         P := Value (Character_Position, J);
         Put (File, Image (P), 1, 0, 1, 0, L1 - 1, J);
      end loop;

      New_Line (File);

      Define (Function_Table_1, Siz, L1, L2);

      case Opt is
         when CPU_Time =>
            Put_Int_Matrix
              (File,
               Array_Img ("T1", Type_Img (Siz),
                          Range_Img (0, L1 - 1),
                          Range_Img (0, L2 - 1, Type_Img (8))),
               Function_Table_1, L1, L2);

         when Memory_Space =>
            Put_Int_Matrix
              (File,
               Array_Img ("T1", Type_Img (Siz),
                          Range_Img (0, L1 - 1)),
               Function_Table_1, L1, 0);
      end case;

      New_Line (File);

      Define (Function_Table_2, Siz, L1, L2);

      case Opt is
         when CPU_Time =>
            Put_Int_Matrix
              (File,
               Array_Img ("T2", Type_Img (Siz),
                          Range_Img (0, L1 - 1),
                          Range_Img (0, L2 - 1, Type_Img (8))),
               Function_Table_2, L1, L2);

         when Memory_Space =>
            Put_Int_Matrix
              (File,
               Array_Img ("T2", Type_Img (Siz),
                          Range_Img (0, L1 - 1)),
               Function_Table_2, L1, 0);
      end case;

      New_Line (File);

      Define (Graph_Table, Siz, L1, L2);
      pragma Assert (L2 = 0);

      Put (File, Array_Img ("G", Type_Img (Siz),
                    Range_Img (0, L1 - 1)));
      New_Line (File);

      for J in 0 .. L1 - 1 loop
         P := Value (Graph_Table, J);
         Put (File, Image (P), 1, 0, 1, 0, L1 - 1, J);
      end loop;

      New_Line (File);

      Put      (File, "   function Hash (S : String) return Natural is");
      New_Line (File);
      Put      (File, "      F : constant Natural := S'First - 1;");
      New_Line (File);
      Put      (File, "      L : constant Natural := S'Length;");
      New_Line (File);
      Put      (File, "      F1, F2 : Natural := 0;");
      New_Line (File);

      Put (File, "      J : ");

      case Opt is
         when CPU_Time =>
            Put (File, Type_Img (8));

         when Memory_Space =>
            Put (File, "Natural");
      end case;

      Put (File, ";");
      New_Line (File);

      Put      (File, "   begin");
      New_Line (File);
      Put      (File, "      for K in P'Range loop");
      New_Line (File);
      Put      (File, "         exit when L < P (K);");
      New_Line (File);
      Put      (File, "         J  := ");

      case Opt is
         when CPU_Time =>
            Put (File, "C");

         when Memory_Space =>
            Put (File, "Character'Pos");
      end case;

      Put      (File, " (S (P (K) + F));");
      New_Line (File);

      Put (File, "         F1 := (F1 + Natural (T1 (K");

      if Opt = CPU_Time then
         Put (File, ", J");
      end if;

      Put (File, "))");

      if Opt = Memory_Space then
         Put (File, " * J");
      end if;

      Put      (File, ") mod ");
      Put      (File, Image (L1));
      Put      (File, ";");
      New_Line (File);

      Put (File, "         F2 := (F2 + Natural (T2 (K");

      if Opt = CPU_Time then
         Put (File, ", J");
      end if;

      Put (File, "))");

      if Opt = Memory_Space then
         Put (File, " * J");
      end if;

      Put      (File, ") mod ");
      Put      (File, Image (L1));
      Put      (File, ";");
      New_Line (File);

      Put      (File, "      end loop;");
      New_Line (File);

      Put      (File,
                "      return (Natural (G (F1)) + Natural (G (F2))) mod ");

      Put      (File, Image (NK));
      Put      (File, ";");
      New_Line (File);
      Put      (File, "   end Hash;");
      New_Line (File);
      New_Line (File);
      Put      (File, "end ");
      Put      (File, Pkg_Name);
      Put      (File, ";");
      New_Line (File);

      if not Use_Stdout then
         Close (File, Status);

         if not Status then
            raise Device_Error;
         end if;
      end if;
   end Produce;

   ---------
   -- Put --
   ---------

   procedure Put (File : File_Descriptor; Str : String) is
      Len : constant Natural := Str'Length;
   begin
      for J in Str'Range loop
         pragma Assert (Str (J) /= ASCII.NUL);
         null;
      end loop;

      if Write (File, Str'Address, Len) /= Len then
         raise Program_Error;
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (F  : File_Descriptor;
      S  : String;
      F1 : Natural;
      L1 : Natural;
      C1 : Natural;
      F2 : Natural;
      L2 : Natural;
      C2 : Natural)
   is
      Len : constant Natural := S'Length;

      procedure Flush;
      --  Write current line, followed by LF

      -----------
      -- Flush --
      -----------

      procedure Flush is
      begin
         Put (F, Line (1 .. Last));
         New_Line (F);
         Last := 0;
      end Flush;

   --  Start of processing for Put

   begin
      if C1 = F1 and then C2 = F2 then
         Last := 0;
      end if;

      if Last + Len + 3 >= Max then
         Flush;
      end if;

      if Last = 0 then
         Add ("     ");

         if F1 <= L1 then
            if C1 = F1 and then C2 = F2 then
               Add ('(');

               if F1 = L1 then
                  Add ("0 .. 0 => ");
               end if;

            else
               Add (' ');
            end if;
         end if;
      end if;

      if C2 = F2 then
         Add ('(');

         if F2 = L2 then
            Add ("0 .. 0 => ");
         end if;

      else
         Add (' ');
      end if;

      Add (S);

      if C2 = L2 then
         Add (')');

         if F1 > L1 then
            Add (';');
            Flush;

         elsif C1 /= L1 then
            Add (',');
            Flush;

         else
            Add (')');
            Add (';');
            Flush;
         end if;

      else
         Add (',');
      end if;
   end Put;

   --------------------
   -- Put_Int_Matrix --
   --------------------

   procedure Put_Int_Matrix
     (File   : File_Descriptor;
      Title  : String;
      Table  : Table_Name;
      Len_1  : Natural;
      Len_2  : Natural)
   is
      F1 : constant Integer := 0;
      L1 : constant Integer := Len_1 - 1;
      F2 : constant Integer := 0;
      L2 : constant Integer := Len_2 - 1;
      Ix : Natural;

   begin
      Put (File, Title);
      New_Line (File);

      if Len_2 = 0 then
         for J in F1 .. L1 loop
            Ix := Value (Table, J, 0);
            Put (File, Image (Ix), 1, 0, 1, F1, L1, J);
         end loop;

      else
         for J in F1 .. L1 loop
            for K in F2 .. L2 loop
               Ix := Value (Table, J, K);
               Put (File, Image (Ix), F1, L1, J, F2, L2, K);
            end loop;
         end loop;
      end if;
   end Put_Int_Matrix;

end GNAT.Perfect_Hash_Generators;
