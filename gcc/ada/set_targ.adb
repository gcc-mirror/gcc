------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E T _ T A R G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2013, Free Software Foundation, Inc.            --
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

with Debug;    use Debug;
with Get_Targ; use Get_Targ;
with Opt;      use Opt;
with Output;   use Output;

with System;        use System;
with System.OS_Lib; use System.OS_Lib;

with Unchecked_Conversion;

package body Set_Targ is

   --------------------------------------------------------
   -- Data Used to Read/Write Target Dependent Info File --
   --------------------------------------------------------

   --  Table of string names written to file

   subtype Str is String;

   S_Bits_BE                    : constant Str := "Bits_BE";
   S_Bits_Per_Unit              : constant Str := "Bits_Per_Unit";
   S_Bits_Per_Word              : constant Str := "Bits_Per_Word";
   S_Bytes_BE                   : constant Str := "Bytes_BE";
   S_Char_Size                  : constant Str := "Char_Size";
   S_Double_Float_Alignment     : constant Str := "Double_Float_Alignment";
   S_Double_Scalar_Alignment    : constant Str := "Double_Scalar_Alignment";
   S_Double_Size                : constant Str := "Double_Size";
   S_Float_Size                 : constant Str := "Float_Size";
   S_Float_Words_BE             : constant Str := "Float_Words_BE";
   S_Int_Size                   : constant Str := "Int_Size";
   S_Long_Double_Size           : constant Str := "Long_Double_Size";
   S_Long_Long_Size             : constant Str := "Long_Long_Size";
   S_Long_Size                  : constant Str := "Long_Size";
   S_Maximum_Alignment          : constant Str := "Maximum_Alignment";
   S_Max_Unaligned_Field        : constant Str := "Max_Unaligned_Field";
   S_Pointer_Size               : constant Str := "Pointer_Size";
   S_Short_Enums                : constant Str := "Short_Enums";
   S_Short_Size                 : constant Str := "Short_Size";
   S_Strict_Alignment           : constant Str := "Strict_Alignment";
   S_System_Allocator_Alignment : constant Str := "System_Allocator_Alignment";
   S_Wchar_T_Size               : constant Str := "Wchar_T_Size";
   S_Words_BE                   : constant Str := "Words_BE";

   --  Table of names

   type AStr is access all String;

   DTN : constant array (Nat range <>) of AStr := (
          S_Bits_BE                    'Unrestricted_Access,
          S_Bits_Per_Unit              'Unrestricted_Access,
          S_Bits_Per_Word              'Unrestricted_Access,
          S_Bytes_BE                   'Unrestricted_Access,
          S_Char_Size                  'Unrestricted_Access,
          S_Double_Float_Alignment     'Unrestricted_Access,
          S_Double_Scalar_Alignment    'Unrestricted_Access,
          S_Double_Size                'Unrestricted_Access,
          S_Float_Size                 'Unrestricted_Access,
          S_Float_Words_BE             'Unrestricted_Access,
          S_Int_Size                   'Unrestricted_Access,
          S_Long_Double_Size           'Unrestricted_Access,
          S_Long_Long_Size             'Unrestricted_Access,
          S_Long_Size                  'Unrestricted_Access,
          S_Maximum_Alignment          'Unrestricted_Access,
          S_Max_Unaligned_Field        'Unrestricted_Access,
          S_Pointer_Size               'Unrestricted_Access,
          S_Short_Enums                'Unrestricted_Access,
          S_Short_Size                 'Unrestricted_Access,
          S_Strict_Alignment           'Unrestricted_Access,
          S_System_Allocator_Alignment 'Unrestricted_Access,
          S_Wchar_T_Size               'Unrestricted_Access,
          S_Words_BE                   'Unrestricted_Access);

   --  Table of corresponding value pointers

   DTV : constant array (Nat range <>) of System.Address := (
          Bits_BE                    'Address,
          Bits_Per_Unit              'Address,
          Bits_Per_Word              'Address,
          Bytes_BE                   'Address,
          Char_Size                  'Address,
          Double_Float_Alignment     'Address,
          Double_Scalar_Alignment    'Address,
          Double_Size                'Address,
          Float_Size                 'Address,
          Float_Words_BE             'Address,
          Int_Size                   'Address,
          Long_Double_Size           'Address,
          Long_Long_Size             'Address,
          Long_Size                  'Address,
          Maximum_Alignment          'Address,
          Max_Unaligned_Field        'Address,
          Pointer_Size               'Address,
          Short_Enums                'Address,
          Short_Size                 'Address,
          Strict_Alignment           'Address,
          System_Allocator_Alignment 'Address,
          Wchar_T_Size               'Address,
          Words_BE                   'Address);

   DTR : array (Nat range DTV'Range) of Boolean := (others => False);
   --  Table of flags used to validate that all values are present in file

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Fail (E : String);
   pragma No_Return (Fail);
   --  Terminate program with fatal error message passed as parameter

   procedure Register_Float_Type
     (Name      : C_String;
      Digs      : Natural;
      Complex   : Boolean;
      Count     : Natural;
      Float_Rep : Float_Rep_Kind;
      Size      : Positive;
      Alignment : Natural);
   pragma Convention (C, Register_Float_Type);
   --  Call back to allow the back end to register available types. This call
   --  back makes entries in the FPT_Mode_Table for any floating point types
   --  reported by the back end. Name is the name of the type as a normal
   --  format Null-terminated string. Digs is the number of digits, where 0
   --  means it is not a fpt type (ignored during registration). Complex is
   --  non-zero if the type has real and imaginary parts (also ignored during
   --  registration). Count is the number of elements in a vector type (zero =
   --  not a vector, registration ignores vectors). Float_Rep shows the kind of
   --  floating-point type, and Size/Alignment are the size/alignment in bits.
   --
   --  So to summarize, the only types that are actually registered have Digs
   --  non-zero, Complex zero (false), and Count zero (not a vector).

   ----------
   -- Fail --
   ----------

   procedure Fail (E : String) is
      E_Fatal : constant := 4;
      --  Code for fatal error
   begin
      Write_Str (E);
      Write_Eol;
      OS_Exit (E_Fatal);
   end Fail;

   -------------------------
   -- Register_Float_Type --
   -------------------------

   procedure Register_Float_Type
     (Name      : C_String;
      Digs      : Natural;
      Complex   : Boolean;
      Count     : Natural;
      Float_Rep : Float_Rep_Kind;
      Size      : Positive;
      Alignment : Natural)
   is
      T    : String (1 .. Name'Length);
      Last : Natural := 0;

      procedure Dump;
      --  Dump information given by the back end for the type to register

      ----------
      -- Dump --
      ----------

      procedure Dump is
      begin
         Write_Str ("type " & T (1 .. Last) & " is ");

         if Count > 0 then
            Write_Str ("array (1 .. ");
            Write_Int (Int (Count));

            if Complex then
               Write_Str (", 1 .. 2");
            end if;

            Write_Str (") of ");

         elsif Complex then
            Write_Str ("array (1 .. 2) of ");
         end if;

         if Digs > 0 then
            Write_Str ("digits ");
            Write_Int (Int (Digs));
            Write_Line (";");

            Write_Str ("pragma Float_Representation (");

            case Float_Rep is
               when IEEE_Binary =>
                  Write_Str ("IEEE");

               when VAX_Native =>
                  case Digs is
                     when  6 =>
                        Write_Str ("VAXF");

                     when  9 =>
                        Write_Str ("VAXD");

                     when 15 =>
                        Write_Str ("VAXG");

                     when others =>
                        Write_Str ("VAX_");
                        Write_Int (Int (Digs));
                  end case;

               when AAMP =>         Write_Str ("AAMP");
            end case;

            Write_Line (", " & T (1 .. Last) & ");");

         else
            Write_Str ("mod 2**");
            Write_Int (Int (Size / Positive'Max (1, Count)));
            Write_Line (";");
         end if;

         Write_Str ("for " & T (1 .. Last) & "'Size use ");
         Write_Int (Int (Size));
         Write_Line (";");

         Write_Str ("for " & T (1 .. Last) & "'Alignment use ");
         Write_Int (Int (Alignment / 8));
         Write_Line (";");
         Write_Eol;
      end Dump;

   --  Start of processing for Register_Float_Type

   begin
      --  Acquire name

      for J in T'Range loop
         T (J) := Name (Name'First + J - 1);

         if T (J) = ASCII.NUL then
            Last := J - 1;
            exit;
         end if;
      end loop;

      --  Dump info if debug flag set

      if Debug_Flag_Dot_B then
         Dump;
      end if;

      --  Acquire entry if non-vector non-complex fpt type (digits non-zero)

      if Digs > 0 and then not Complex and then Count = 0 then
         Num_FPT_Modes := Num_FPT_Modes + 1;
         FPT_Mode_Table (Num_FPT_Modes) :=
           (NAME      => new String'(T (1 .. Last)),
            DIGS      => Digs,
            FLOAT_REP => Float_Rep,
            SIZE      => Size,
            ALIGNMENT => Alignment);
      end if;
   end Register_Float_Type;

   -----------------------------------
   -- Write_Target_Dependent_Values --
   -----------------------------------

   --  We do this at the System.Os_Lib level, since we have to do the read at
   --  that level anyway, so it is easier and more consistent to follow the
   --  same path for the write.

   procedure Write_Target_Dependent_Values is
      Fdesc  : File_Descriptor;
      OK     : Boolean;

      Buffer : String (1 .. 80);
      Buflen : Natural;
      --  Buffer used to build line one of file

      type ANat is access all Natural;
      --  Pointer to Nat or Pos value (it is harmless to treat Pos values and
      --  Nat values as Natural via Unchecked_Conversion).

      function To_ANat is new Unchecked_Conversion (Address, ANat);

      procedure AddC (C : Character);
      --  Add one character to buffer

      procedure AddN (N : Natural);
      --  Add representation of integer N to Buffer, updating Buflen. N
      --  must be less than 1000, and output is 3 characters with leading
      --  spaces as needed.

      procedure Write_Line;
      --  Output contents of Buffer (1 .. Buflen) followed by a New_Line,
      --  and set Buflen back to zero, ready to write next line.

      ----------
      -- AddC --
      ----------

      procedure AddC (C : Character) is
      begin
         Buflen := Buflen + 1;
         Buffer (Buflen) := C;
      end AddC;

      ----------
      -- AddN --
      ----------

      procedure AddN (N : Natural) is
      begin
         if N > 999 then
            raise Program_Error;
         end if;

         if N > 99 then
            AddC (Character'Val (48 + N / 100));
         else
            AddC (' ');
         end if;

         if N > 9 then
            AddC (Character'Val (48 + N / 10 mod 10));
         else
            AddC (' ');
         end if;

         AddC (Character'Val (48 + N mod 10));
      end AddN;

      ----------------
      -- Write_Line --
      ----------------

      procedure Write_Line is
      begin
         AddC (ASCII.LF);

         if Buflen /= Write (Fdesc, Buffer'Address, Buflen) then
            Delete_File (Target_Dependent_Info_Write_Name'Address, OK);
            Fail ("disk full writing file "
                  & Target_Dependent_Info_Write_Name.all);
         end if;

         Buflen := 0;
      end Write_Line;

   --  Start of processing for Write_Target_Dependent_Values

   begin
      Fdesc :=
        Create_File (Target_Dependent_Info_Write_Name.all'Address, Text);

      if Fdesc = Invalid_FD then
         Fail ("cannot create file " & Target_Dependent_Info_Write_Name.all);
      end if;

      --  Loop through values

      for J in DTN'Range loop

         --  Output name

         Buflen := DTN (J)'Length;
         Buffer (1 .. Buflen) := DTN (J).all;

         --  Line up values

         while Buflen < 26 loop
            AddC (' ');
         end loop;

         AddC (' ');
         AddC (' ');

         --  Output value and write line

         AddN (To_ANat (DTV (J)).all);
         Write_Line;
      end loop;

      --  Blank line to separate sections

      Write_Line;

      --  Write lines for registered FPT types

      for J in 1 .. Num_FPT_Modes loop
         declare
            E : FPT_Mode_Entry renames FPT_Mode_Table (J);
         begin
            Buflen := E.NAME'Last;
            Buffer (1 .. Buflen) := E.NAME.all;

            --  Pad out to line up values

            while Buflen < 11 loop
               AddC (' ');
            end loop;

            AddC (' ');
            AddC (' ');

            AddN (E.DIGS);
            AddC (' ');
            AddC (' ');

            case E.FLOAT_REP is
               when IEEE_Binary =>
                  AddC ('I');
               when VAX_Native  =>
                  AddC ('V');
               when AAMP        =>
                  AddC ('A');
            end case;

            AddC (' ');

            AddN (E.SIZE);
            AddC (' ');

            AddN (E.ALIGNMENT);
            Write_Line;
         end;
      end loop;

      --  Close file

      Close (Fdesc, OK);

      if not OK then
         Fail ("disk full writing file "
               & Target_Dependent_Info_Write_Name.all);
      end if;
   end Write_Target_Dependent_Values;

--  Package Initialization, set target dependent values. This must be done
--  early on, before we start accessing various compiler packages, since
--  these values are used all over the place.

begin
   --  First step: see if the -gnateT switch is present. As we have noted,
   --  this has to be done very early, so can not depend on the normal circuit
   --  for reading switches and setting switches in Opt. The following code
   --  will set Opt.Target_Dependent_Info_Read_Name if the switch -gnateT=name
   --  is present in the options string.

   declare
      type Arg_Array is array (Nat) of Big_String_Ptr;
      type Arg_Array_Ptr is access Arg_Array;
      --  Types to access compiler arguments

      save_argc : Nat;
      pragma Import (C, save_argc);
      --  Saved value of argc (number of arguments), imported from misc.c

      save_argv : Arg_Array_Ptr;
      pragma Import (C, save_argv);
      --  Saved value of argv (argument pointers), imported from misc.c

      gnat_argc : Nat;
      gnat_argv : Arg_Array_Ptr;
      pragma Import (C, gnat_argc);
      pragma Import (C, gnat_argv);
      --  If save_argv is not set, default to gnat_argc/argv

      argc : Nat;
      argv : Arg_Array_Ptr;

      function Len_Arg (Arg : Big_String_Ptr) return Nat;
      --  Determine length of argument Arg (a nul terminated C string).

      -------------
      -- Len_Arg --
      -------------

      function Len_Arg (Arg : Big_String_Ptr) return Nat is
      begin
         for J in 1 .. Nat'Last loop
            if Arg (Natural (J)) = ASCII.NUL then
               return J - 1;
            end if;
         end loop;

         raise Program_Error;
      end Len_Arg;

   begin
      if save_argv /= null then
         argv := save_argv;
         argc := save_argc;
      else
         --  Case of a non gcc compiler, e.g. gnat2why or gnat2scil
         argv := gnat_argv;
         argc := gnat_argc;
      end if;

      --  Loop through arguments looking for -gnateT, also look for -gnatd.b

      for Arg in 1 .. argc - 1 loop
         declare
            Argv_Ptr : constant Big_String_Ptr := argv (Arg);
            Argv_Len : constant Nat            := Len_Arg (Argv_Ptr);

         begin
            if Argv_Len > 8
              and then Argv_Ptr (1 .. 8) = "-gnateT="
            then
               Opt.Target_Dependent_Info_Read_Name :=
                 new String'(Argv_Ptr (9 .. Natural (Argv_Len)));

            elsif Argv_Len >= 8
              and then Argv_Ptr (1 .. 8) = "-gnatd.b"
            then
               Debug_Flag_Dot_B := True;
            end if;
         end;
      end loop;
   end;

   --  If the switch is not set, we get all values from the back end

   if Opt.Target_Dependent_Info_Read_Name = null then

      --  Set values by direct calls to the back end

      Bits_BE                    := Get_Bits_BE;
      Bits_Per_Unit              := Get_Bits_Per_Unit;
      Bits_Per_Word              := Get_Bits_Per_Word;
      Bytes_BE                   := Get_Bytes_BE;
      Char_Size                  := Get_Char_Size;
      Double_Float_Alignment     := Get_Double_Float_Alignment;
      Double_Scalar_Alignment    := Get_Double_Scalar_Alignment;
      Double_Size                := Get_Double_Size;
      Float_Size                 := Get_Float_Size;
      Float_Words_BE             := Get_Float_Words_BE;
      Int_Size                   := Get_Int_Size;
      Long_Double_Size           := Get_Long_Double_Size;
      Long_Long_Size             := Get_Long_Long_Size;
      Long_Size                  := Get_Long_Size;
      Maximum_Alignment          := Get_Maximum_Alignment;
      Max_Unaligned_Field        := Get_Max_Unaligned_Field;
      Pointer_Size               := Get_Pointer_Size;
      Short_Enums                := Get_Short_Enums;
      Short_Size                 := Get_Short_Size;
      Strict_Alignment           := Get_Strict_Alignment;
      System_Allocator_Alignment := Get_System_Allocator_Alignment;
      Wchar_T_Size               := Get_Wchar_T_Size;
      Words_BE                   := Get_Words_BE;

      --  Register floating-point types from the back end

      Register_Back_End_Types (Register_Float_Type'Access);

   --  Case of reading the target dependent values from file

   --  This is bit more complex than might be expected, because it has to be
   --  done very early. All kinds of packages depend on these values, and we
   --  can't wait till the normal processing of reading command line switches
   --  etc to read the file. We do this at the System.OS_Lib level since it is
   --  too early to be using Osint directly.

   else
      Read_Target_Dependent_Values : declare
         File_Desc : File_Descriptor;
         N         : Natural;

         type ANat is access all Natural;
         --  Pointer to Nat or Pos value (it is harmless to treat Pos values
         --  as Nat via Unchecked_Conversion).

         function To_ANat is new Unchecked_Conversion (Address, ANat);

         VP : ANat;

         Buffer : String (1 .. 2000);
         Buflen : Natural;
         --  File information and length (2000 easily enough)

         Nam_Buf : String (1 .. 40);
         Nam_Len : Natural;

         procedure Check_Spaces;
         --  Checks that we have one or more spaces and skips them

         procedure FailN (S : String);
         --  Calls Fail adding " name in file xxx", where name is the currently
         --  gathered name in Nam_Buf, surrounded by quotes, and xxx is the
         --  name of the file.

         procedure Get_Name;
         --  Scan out name, leaving it in Nam_Buf with Nam_Len set. Calls
         --  Skip_Spaces to skip any following spaces. Note that the name is
         --  terminated by a sequence of at least two spaces.

         function Get_Nat return Natural;
         --  N on entry points to decimal integer, scan out decimal integer
         --  and return it, leaving N pointing to following space or LF.

         procedure Skip_Spaces;
         --  Skip past spaces

         ------------------
         -- Check_Spaces --
         ------------------

         procedure Check_Spaces is
         begin
            if N > Buflen or else Buffer (N) /= ' ' then
               FailN ("missing space for");
            end if;

            Skip_Spaces;
            return;
         end Check_Spaces;

         -----------
         -- FailN --
         -----------

         procedure FailN (S : String) is
         begin
            Fail (S & " """ & Nam_Buf (1 .. Nam_Len) & """ in file "
                  & Target_Dependent_Info_Read_Name.all);
         end FailN;

         --------------
         -- Get_Name --
         --------------

         procedure Get_Name is
         begin
            Nam_Len := 0;

            --  Scan out name and put it in Nam_Buf

            loop
               if N > Buflen or else Buffer (N) = ASCII.LF then
                  FailN ("incorrectly formatted line for");
               end if;

               --  Name is terminated by two blanks

               exit when N < Buflen and then Buffer (N .. N + 1) = "  ";

               Nam_Len := Nam_Len + 1;

               if Nam_Len > Nam_Buf'Last then
                  Fail ("name too long");
               end if;

               Nam_Buf (Nam_Len) := Buffer (N);
               N := N + 1;
            end loop;

            Check_Spaces;
         end Get_Name;

         -------------
         -- Get_Nat --
         -------------

         function Get_Nat return Natural is
            Result : Natural := 0;

         begin
            loop
               if N > Buflen
                 or else Buffer (N) not in '0' .. '9'
                 or else Result > 999
               then
                  FailN ("bad value for");
               end if;

               Result := Result * 10 + (Character'Pos (Buffer (N)) - 48);
               N := N + 1;

               exit when N <= Buflen
                 and then (Buffer (N) = ASCII.LF or else Buffer (N) = ' ');
            end loop;

            return Result;
         end Get_Nat;

         -----------------
         -- Skip_Spaces --
         -----------------

         procedure Skip_Spaces is
         begin
            while N <= Buflen and Buffer (N) = ' ' loop
               N := N + 1;
            end loop;
         end Skip_Spaces;

      --  Start of processing for Read_Target_Dependent_Values

      begin
         File_Desc := Open_Read (Target_Dependent_Info_Read_Name.all, Text);

         if File_Desc = Invalid_FD then
            Fail ("cannot read file " & Target_Dependent_Info_Read_Name.all);
         end if;

         Buflen := Read (File_Desc, Buffer'Address, Buffer'Length);

         if Buflen = Buffer'Length then
            Fail ("file is too long: " & Target_Dependent_Info_Read_Name.all);
         end if;

         --  Scan through file for properly formatted entries in first section

         N := 1;
         while N <= Buflen and then Buffer (N) /= ASCII.LF loop
            Get_Name;

            --  Validate name and get corresponding value pointer

            VP := null;

            for J in DTN'Range loop
               if DTN (J).all = Nam_Buf (1 .. Nam_Len) then
                  VP := To_ANat (DTV (J));
                  DTR (J) := True;
                  exit;
               end if;
            end loop;

            if VP = null then
               FailN ("unrecognized name");
            end if;

            --  Scan out value

            VP.all := Get_Nat;

            if N > Buflen or else Buffer (N) /= ASCII.LF then
               FailN ("misformatted line for");
            end if;

            N := N + 1; -- skip LF
         end loop;

         --  Fall through this loop when all lines in first section read.
         --  Check that values have been supplied for all entries.

         for J in DTR'Range loop
            if not DTR (J) then
               Fail ("missing entry for " & DTN (J).all & " in file "
                     & Target_Dependent_Info_Read_Name.all);
            end if;
         end loop;

         --  Now acquire FPT entries

         if N >= Buflen then
            Fail ("missing entries for FPT modes in file "
                  & Target_Dependent_Info_Read_Name.all);
         end if;

         if Buffer (N) = ASCII.LF then
            N := N + 1;
         else
            Fail ("missing blank line in file "
                  & Target_Dependent_Info_Read_Name.all);
         end if;

         Num_FPT_Modes := 0;
         while N <= Buflen loop
            Get_Name;

            Num_FPT_Modes := Num_FPT_Modes + 1;

            declare
               E : FPT_Mode_Entry renames FPT_Mode_Table (Num_FPT_Modes);

            begin
               E.NAME := new String'(Nam_Buf (1 .. Nam_Len));

               E.DIGS := Get_Nat;
               Check_Spaces;

               case Buffer (N) is
                  when 'I'    =>
                     E.FLOAT_REP := IEEE_Binary;
                  when 'V'    =>
                     E.FLOAT_REP := VAX_Native;
                  when 'A'    =>
                     E.FLOAT_REP := AAMP;
                  when others =>
                     FailN ("bad float rep field for");
               end case;

               N := N + 1;
               Check_Spaces;

               E.SIZE := Get_Nat;
               Check_Spaces;

               E.ALIGNMENT := Get_Nat;

               if Buffer (N) /= ASCII.LF then
                  FailN ("junk at end of line for");
               end if;

               N := N + 1;
            end;
         end loop;
      end Read_Target_Dependent_Values;
   end if;
end Set_Targ;
