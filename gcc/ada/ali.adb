------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                  A L I                                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6 $
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

with Butil;   use Butil;
with Debug;   use Debug;
with Fname;   use Fname;
with Namet;   use Namet;
with Osint;   use Osint;
with Output;  use Output;

package body ALI is

   use ASCII;
   --  Make control characters visible

   --------------------
   -- Initialize_ALI --
   --------------------

   procedure Initialize_ALI is
   begin
      --  When (re)initializing ALI data structures the ALI user expects to
      --  get a fresh set of data structures. Thus we first need to erase the
      --  marks put in the name table by the previous set of ALI routine calls.
      --  This loop is empty and harmless the first time in.

      for J in ALIs.First .. ALIs.Last loop
         Set_Name_Table_Info (ALIs.Table (J).Afile, 0);
      end loop;

      ALIs.Init;
      Units.Init;
      Withs.Init;
      Sdep.Init;
      Linker_Options.Init;
      Xref_Section.Init;
      Xref_Entity.Init;
      Xref.Init;
      Version_Ref.Reset;

      --  Add dummy zero'th item in Linker_Options for the sort function

      Linker_Options.Increment_Last;

      --  Initialize global variables recording cumulative options in all
      --  ALI files that are read for a given processing run in gnatbind.

      Dynamic_Elaboration_Checks_Specified := False;
      Float_Format_Specified               := ' ';
      Locking_Policy_Specified             := ' ';
      No_Normalize_Scalars_Specified       := False;
      No_Object_Specified                  := False;
      Normalize_Scalars_Specified          := False;
      No_Run_Time_Specified                := False;
      Queuing_Policy_Specified             := ' ';
      Static_Elaboration_Model_Used        := False;
      Task_Dispatching_Policy_Specified    := ' ';
      Unreserve_All_Interrupts_Specified   := False;
      Zero_Cost_Exceptions_Specified       := False;

   end Initialize_ALI;

   --------------
   -- Scan_ALI --
   --------------

   function Scan_ALI
     (F         : File_Name_Type;
      T         : Text_Buffer_Ptr;
      Ignore_ED : Boolean;
      Err       : Boolean;
      Read_Xref : Boolean := False)
      return      ALI_Id
   is
      P         : Text_Ptr := T'First;
      Line      : Logical_Line_Number := 1;
      Id        : ALI_Id;
      C         : Character;
      NS_Found  : Boolean;
      First_Arg : Arg_Id;

      function At_Eol return Boolean;
      --  Test if at end of line

      function At_End_Of_Field return Boolean;
      --  Test if at end of line, or if at blank or horizontal tab

      procedure Check_At_End_Of_Field;
      --  Check if we are at end of field, fatal error if not

      procedure Checkc (C : Character);
      --  Check next character is C. If so bump past it, if not fatal error

      Bad_ALI_Format : exception;

      procedure Fatal_Error;
      --  Generate fatal error message for badly formatted ALI file if
      --  Err is false, or raise Bad_ALI_Format if Err is True.

      function Getc return Character;
      --  Get next character, bumping P past the character obtained

      function Get_Name (Lower : Boolean := False) return Name_Id;
      --  Skip blanks, then scan out a name (name is left in Name_Buffer with
      --  length in Name_Len, as well as being returned in Name_Id form). The
      --  name is adjusted appropriately if it refers to a file that is to be
      --  substituted by another name as a result of a configuration pragma.
      --  If Lower is set to true then the Name_Buffer will be converted to
      --  all lower case. This only happends for systems where file names are
      --  not case sensitive, and ensures that gnatbind works correctly on
      --  such systems, regardless of the case of the file name. Note that
      --  a name can be terminated by a right typeref bracket or '='.

      function Get_Nat return Nat;
      --  Skip blanks, then scan out an unsigned integer value in Nat range

      function Get_Stamp return Time_Stamp_Type;
      --  Skip blanks, then scan out a time stamp

      function Nextc return Character;
      --  Return current character without modifying pointer P

      procedure Skip_Eol;
      --  Skip past end of line (fatal error if not at end of line)

      procedure Skip_Space;
      --  Skip past white space (blanks or horizontal tab)

      ---------------------
      -- At_End_Of_Field --
      ---------------------

      function At_End_Of_Field return Boolean is
      begin
         return Nextc <= ' ';
      end At_End_Of_Field;

      ------------
      -- At_Eol --
      ------------

      function At_Eol return Boolean is
      begin
         return Nextc = EOF or else Nextc = CR or else Nextc = LF;
      end At_Eol;

      ---------------------------
      -- Check_At_End_Of_Field --
      ---------------------------

      procedure Check_At_End_Of_Field is
      begin
         if not At_End_Of_Field then
            Fatal_Error;
         end if;
      end Check_At_End_Of_Field;

      ------------
      -- Checkc --
      ------------

      procedure Checkc (C : Character) is
      begin
         if Nextc = C then
            P := P + 1;
         else
            Fatal_Error;
         end if;
      end Checkc;

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error is
         Ptr1 : Text_Ptr;
         Ptr2 : Text_Ptr;
         Col  : Int;

         procedure Wchar (C : Character);
         --  Write a single character, replacing horizontal tab by spaces

         procedure Wchar (C : Character) is
         begin
            if C = HT then
               loop
                  Wchar (' ');
                  exit when Col mod 8 = 0;
               end loop;

            else
               Write_Char (C);
               Col := Col + 1;
            end if;
         end Wchar;

      --  Start of processing for Fatal_Error

      begin
         if Err then
            raise Bad_ALI_Format;
         end if;

         Set_Standard_Error;
         Write_Str ("fatal error: file ");
         Write_Name (F);
         Write_Str (" is incorrectly formatted");
         Write_Eol;
         Write_Str
           ("make sure you are using consistent versions of gcc/gnatbind");
         Write_Eol;

         --  Find start of line

         Ptr1 := P;

         while Ptr1 > T'First
           and then T (Ptr1 - 1) /= CR
           and then T (Ptr1 - 1) /= LF
         loop
            Ptr1 := Ptr1 - 1;
         end loop;

         Write_Int (Int (Line));
         Write_Str (". ");

         if Line < 100 then
            Write_Char (' ');
         end if;

         if Line < 10 then
            Write_Char (' ');
         end if;

         Col := 0;
         Ptr2 := Ptr1;

         while Ptr2 < T'Last
           and then T (Ptr2) /= CR
           and then T (Ptr2) /= LF
         loop
            Wchar (T (Ptr2));
            Ptr2 := Ptr2 + 1;
         end loop;

         Write_Eol;

         Write_Str ("     ");
         Col := 0;

         while Ptr1 < P loop
            if T (Ptr1) = HT then
               Wchar (HT);
            else
               Wchar (' ');
            end if;

            Ptr1 := Ptr1 + 1;
         end loop;

         Wchar ('|');
         Write_Eol;

         Exit_Program (E_Fatal);
      end Fatal_Error;

      --------------
      -- Get_Name --
      --------------

      function Get_Name (Lower : Boolean := False) return Name_Id is
      begin
         Name_Len := 0;
         Skip_Space;

         if At_Eol then
            Fatal_Error;
         end if;

         loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Getc;
            exit when At_End_Of_Field
              or else Nextc = ')'
              or else Nextc = '}'
              or else Nextc = '>'
              or else Nextc = '=';
         end loop;

         --  Convert file name to all lower case if file names are not case
         --  sensitive. This ensures that we handle names in the canonical
         --  lower case format, regardless of the actual case.

         if Lower and not File_Names_Case_Sensitive then
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
         end if;

         return Name_Find;
      end Get_Name;

      -------------
      -- Get_Nat --
      -------------

      function Get_Nat return Nat is
         V : Nat;

      begin
         Skip_Space;

         V := 0;

         loop
            V := V * 10 + (Character'Pos (Getc) - Character'Pos ('0'));
            exit when At_End_Of_Field;
            exit when Nextc < '0' or Nextc > '9';
         end loop;

         return V;
      end Get_Nat;

      ---------------
      -- Get_Stamp --
      ---------------

      function Get_Stamp return Time_Stamp_Type is
         T     : Time_Stamp_Type;
         Start : Integer;

      begin
         Skip_Space;

         if At_Eol then
            Fatal_Error;
         end if;

         --  Following reads old style time stamp missing first two digits

         if Nextc in '7' .. '9' then
            T (1) := '1';
            T (2) := '9';
            Start := 3;

         --  Normal case of full year in time stamp

         else
            Start := 1;
         end if;

         for J in Start .. T'Last loop
            T (J) := Getc;
         end loop;

         return T;
      end Get_Stamp;

      ----------
      -- Getc --
      ----------

      function Getc return Character is
      begin
         if P = T'Last then
            return EOF;
         else
            P := P + 1;
            return T (P - 1);
         end if;
      end Getc;

      -----------
      -- Nextc --
      -----------

      function Nextc return Character is
      begin
         return T (P);
      end Nextc;

      --------------
      -- Skip_Eol --
      --------------

      procedure Skip_Eol is
      begin
         Skip_Space;
         if not At_Eol then Fatal_Error; end if;

         --  Loop to skip past blank lines (first time through skips this EOL)

         while Nextc < ' ' and then Nextc /= EOF loop
            if Nextc = LF then
               Line := Line + 1;
            end if;

            P := P + 1;
         end loop;
      end Skip_Eol;

      ----------------
      -- Skip_Space --
      ----------------

      procedure Skip_Space is
      begin
         while Nextc = ' ' or else Nextc = HT loop
            P := P + 1;
         end loop;
      end Skip_Space;

   --------------------------------------
   -- Start of processing for Scan_ALI --
   --------------------------------------

   begin
      ALIs.Increment_Last;
      Id := ALIs.Last;
      Set_Name_Table_Info (F, Int (Id));

      ALIs.Table (Id) := (
        Afile                      => F,
        Compile_Errors             => False,
        First_Sdep                 => No_Sdep_Id,
        First_Unit                 => No_Unit_Id,
        Float_Format               => 'I',
        Last_Sdep                  => No_Sdep_Id,
        Last_Unit                  => No_Unit_Id,
        Locking_Policy             => ' ',
        Main_Priority              => -1,
        Main_Program               => None,
        No_Object                  => False,
        No_Run_Time                => False,
        Normalize_Scalars          => False,
        Ofile_Full_Name            => Full_Object_File_Name,
        Queuing_Policy             => ' ',
        Restrictions               => (others => ' '),
        Sfile                      => No_Name,
        Task_Dispatching_Policy    => ' ',
        Time_Slice_Value           => -1,
        WC_Encoding                => '8',
        Unit_Exception_Table       => False,
        Ver                        => (others => ' '),
        Ver_Len                    => 0,
        Zero_Cost_Exceptions       => False);

      --  Acquire library version

      Checkc ('V');
      Checkc (' ');
      Skip_Space;
      Checkc ('"');

      for J in 1 .. Ver_Len_Max loop
         C := Getc;
         exit when C = '"';
         ALIs.Table (Id).Ver (J) := C;
         ALIs.Table (Id).Ver_Len := J;
      end loop;

      Skip_Eol;

      --  Acquire main program line if present

      C := Getc;

      if C = 'M' then
         Checkc (' ');
         Skip_Space;

         C := Getc;

         if C = 'F' then
            ALIs.Table (Id).Main_Program := Func;
         elsif C = 'P' then
            ALIs.Table (Id).Main_Program := Proc;
         else
            P := P - 1;
            Fatal_Error;
         end if;

         Skip_Space;

         if not At_Eol then
            if Nextc < 'A' then
               ALIs.Table (Id).Main_Priority := Get_Nat;
            end if;

            Skip_Space;

            if Nextc = 'T' then
               P := P + 1;
               Checkc ('=');
               ALIs.Table (Id).Time_Slice_Value := Get_Nat;
            end if;

            Skip_Space;

            Checkc ('W');
            Checkc ('=');
            ALIs.Table (Id).WC_Encoding := Getc;
         end if;

         Skip_Eol;
         C := Getc;

      end if;

      --  Acquire argument lines

      First_Arg := Args.Last + 1;

      Arg_Loop : while C = 'A' loop
         Checkc (' ');
         Name_Len := 0;

         while not At_Eol loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Getc;
         end loop;

         Args.Increment_Last;
         Args.Table (Args.Last) := new String'(Name_Buffer (1 .. Name_Len));

         Skip_Eol;
         C := Getc;
      end loop Arg_Loop;

      --  Acquire P line, first set defaults

      if C /= 'P' then
         Fatal_Error;
      end if;

      NS_Found := False;

      while not At_Eol loop
         Checkc (' ');
         Skip_Space;
         C := Getc;

         if C = 'C' then
            Checkc ('E');
            ALIs.Table (Id).Compile_Errors := True;

         elsif C = 'F' then
            Float_Format_Specified := Getc;
            ALIs.Table (Id).Float_Format := Float_Format_Specified;

         elsif C = 'L' then
            Locking_Policy_Specified := Getc;
            ALIs.Table (Id).Locking_Policy := Locking_Policy_Specified;

         elsif C = 'N' then
            C := Getc;

            if C = 'O' then
               ALIs.Table (Id).No_Object := True;
               No_Object_Specified := True;

            elsif C = 'R' then
               No_Run_Time_Specified := True;
               ALIs.Table (Id).No_Run_Time := True;

            elsif C = 'S' then
               ALIs.Table (Id).Normalize_Scalars := True;
               Normalize_Scalars_Specified := True;
               NS_Found := True;

            else
               Fatal_Error;
            end if;

         elsif C = 'Q' then
            Queuing_Policy_Specified := Getc;
            ALIs.Table (Id).Queuing_Policy := Queuing_Policy_Specified;

         elsif C = 'T' then
            Task_Dispatching_Policy_Specified := Getc;
            ALIs.Table (Id).Task_Dispatching_Policy :=
              Task_Dispatching_Policy_Specified;

         elsif C = 'U' then
            if Nextc = 'A' then
               Unreserve_All_Interrupts_Specified := True;
               C := Getc;

            else
               Checkc ('X');
               ALIs.Table (Id).Unit_Exception_Table := True;
            end if;

         elsif C = 'Z' then
            Checkc ('X');
               ALIs.Table (Id).Zero_Cost_Exceptions := True;
               Zero_Cost_Exceptions_Specified := True;

         else
            Fatal_Error;
         end if;
      end loop;

      if not NS_Found then
         No_Normalize_Scalars_Specified := True;
      end if;

      Skip_Eol;

      --  Acquire restrictions line

      if Getc /= 'R' then
         Fatal_Error;

      else
         Checkc (' ');
         Skip_Space;

         for J in Partition_Restrictions loop
            C := Getc;

            if C = 'v' or else C = 'r' or else C = 'n' then
               ALIs.Table (Id).Restrictions (J) := C;
            else
               Fatal_Error;
            end if;
         end loop;

         if At_Eol then
            Skip_Eol;
            C := Getc;
         else
            Fatal_Error;
         end if;
      end if;

      --  Loop to acquire unit entries

      Unit_Loop : while C = 'U' loop
         Checkc (' ');
         Skip_Space;
         Units.Increment_Last;

         if ALIs.Table (Id).First_Unit = No_Unit_Id then
            ALIs.Table (Id).First_Unit := Units.Last;
         end if;

         Units.Table (Units.Last).Uname           := Get_Name;
         Units.Table (Units.Last).Predefined      := Is_Predefined_Unit;
         Units.Table (Units.Last).Internal        := Is_Internal_Unit;
         Units.Table (Units.Last).My_ALI          := Id;
         Units.Table (Units.Last).Sfile           := Get_Name (Lower => True);
         Units.Table (Units.Last).Pure            := False;
         Units.Table (Units.Last).Preelab         := False;
         Units.Table (Units.Last).No_Elab         := False;
         Units.Table (Units.Last).Shared_Passive  := False;
         Units.Table (Units.Last).RCI             := False;
         Units.Table (Units.Last).Remote_Types    := False;
         Units.Table (Units.Last).Has_RACW        := False;
         Units.Table (Units.Last).Init_Scalars    := False;
         Units.Table (Units.Last).Is_Generic      := False;
         Units.Table (Units.Last).Icasing         := Mixed_Case;
         Units.Table (Units.Last).Kcasing         := All_Lower_Case;
         Units.Table (Units.Last).Dynamic_Elab    := False;
         Units.Table (Units.Last).Elaborate_Body  := False;
         Units.Table (Units.Last).Set_Elab_Entity := False;
         Units.Table (Units.Last).Version         := "00000000";
         Units.Table (Units.Last).First_With      := Withs.Last + 1;
         Units.Table (Units.Last).First_Arg       := First_Arg;
         Units.Table (Units.Last).Elab_Position   := 0;

         if Debug_Flag_U then
            Write_Str (" ----> reading unit ");
            Write_Unit_Name (Units.Table (Units.Last).Uname);
            Write_Str (" from file ");
            Write_Name (Units.Table (Units.Last).Sfile);
            Write_Eol;
         end if;

         --  Check for duplicated unit in different files

         declare
            Info : constant Int := Get_Name_Table_Info
                                     (Units.Table (Units.Last).Uname);
         begin
            if Info /= 0
              and then Units.Table (Units.Last).Sfile /=
                       Units.Table (Unit_Id (Info)).Sfile
            then
               --  If Err is set then treat duplicate unit name as an instance
               --  of a bad ALI format. This is the case of being called from
               --  gnatmake, and the point is that if anything is wrong with
               --  the ALI file, then gnatmake should just recompile.

               if Err then
                  raise Bad_ALI_Format;

               --  If Err is not set, then this is a fatal error

               else
                  Set_Standard_Error;
                  Write_Str ("error: duplicate unit name: ");
                  Write_Eol;

                  Write_Str ("error: unit """);
                  Write_Unit_Name (Units.Table (Units.Last).Uname);
                  Write_Str (""" found in file """);
                  Write_Name_Decoded (Units.Table (Units.Last).Sfile);
                  Write_Char ('"');
                  Write_Eol;

                  Write_Str ("error: unit """);
                  Write_Unit_Name (Units.Table (Unit_Id (Info)).Uname);
                  Write_Str (""" found in file """);
                  Write_Name_Decoded (Units.Table (Unit_Id (Info)).Sfile);
                  Write_Char ('"');
                  Write_Eol;

                  Exit_Program (E_Fatal);
               end if;
            end if;
         end;

         Set_Name_Table_Info
           (Units.Table (Units.Last).Uname, Int (Units.Last));

         --  Scan out possible version and other parameters

         loop
            Skip_Space;
            exit when At_Eol;
            C := Getc;

            --  Version field

            if C in '0' .. '9' or else C in 'a' .. 'f' then
               Units.Table (Units.Last).Version (1) := C;

               for J in 2 .. 8 loop
                  C := Getc;
                  Units.Table (Units.Last).Version (J) := C;
               end loop;

            --  DE parameter (Dynamic elaboration checks

            elsif C = 'D' then
               Checkc ('E');
               Check_At_End_Of_Field;
               Units.Table (Units.Last).Dynamic_Elab := True;
               Dynamic_Elaboration_Checks_Specified := True;

            --  EB/EE parameters

            elsif C = 'E' then
               C := Getc;

               if C = 'B' then
                  Units.Table (Units.Last).Elaborate_Body := True;

               elsif C = 'E' then
                  Units.Table (Units.Last).Set_Elab_Entity := True;

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            --  GE parameter (generic)

            elsif C = 'G' then
               Checkc ('E');
               Check_At_End_Of_Field;
               Units.Table (Units.Last).Is_Generic := True;

            --  IL/IS/IU parameters

            elsif C = 'I' then
               C := Getc;

               if C = 'L' then
                  Units.Table (Units.Last).Icasing := All_Lower_Case;

               elsif C = 'S' then
                  Units.Table (Units.Last).Init_Scalars := True;
                  Initialize_Scalars_Used := True;

               elsif C = 'U' then
                  Units.Table (Units.Last).Icasing := All_Upper_Case;

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            --  KM/KU parameters

            elsif C = 'K' then
               C := Getc;

               if C = 'M' then
                  Units.Table (Units.Last).Kcasing := Mixed_Case;

               elsif C = 'U' then
                  Units.Table (Units.Last).Kcasing := All_Upper_Case;

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            --  NE parameter

            elsif C = 'N' then
               Checkc ('E');
               Units.Table (Units.Last).No_Elab := True;
               Check_At_End_Of_Field;

            --  PR/PU/PK parameters

            elsif C = 'P' then
               C := Getc;

               --  PR parameter (preelaborate)

               if C = 'R' then
                  Units.Table (Units.Last).Preelab := True;

               --  PU parameter (pure)

               elsif C = 'U' then
                  Units.Table (Units.Last).Pure := True;

               --  PK indicates unit is package

               elsif C = 'K' then
                  Units.Table (Units.Last).Unit_Kind := 'p';

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            --  RC/RT parameters

            elsif C = 'R' then
               C := Getc;

               --  RC parameter (remote call interface)

               if C = 'C' then
                  Units.Table (Units.Last).RCI := True;

               --  RT parameter (remote types)

               elsif C = 'T' then
                  Units.Table (Units.Last).Remote_Types := True;

               --  RA parameter (remote access to class wide type)

               elsif C = 'A' then
                  Units.Table (Units.Last).Has_RACW := True;

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            elsif C = 'S' then
               C := Getc;

               --  SP parameter (shared passive)

               if C = 'P' then
                  Units.Table (Units.Last).Shared_Passive := True;

               --  SU parameter indicates unit is subprogram

               elsif C = 'U' then
                  Units.Table (Units.Last).Unit_Kind := 's';

               else
                  Fatal_Error;
               end if;

               Check_At_End_Of_Field;

            else
               Fatal_Error;
            end if;

         end loop;

         Skip_Eol;

         --  Check if static elaboration model used

         if not Units.Table (Units.Last).Dynamic_Elab
           and then not Units.Table (Units.Last).Internal
         then
            Static_Elaboration_Model_Used := True;
         end if;

         --  Scan out With lines for this unit

         C := Getc;

         With_Loop : while C = 'W' loop
            Checkc (' ');
            Skip_Space;
            Withs.Increment_Last;
            Withs.Table (Withs.Last).Uname              := Get_Name;
            Withs.Table (Withs.Last).Elaborate          := False;
            Withs.Table (Withs.Last).Elaborate_All      := False;
            Withs.Table (Withs.Last).Elab_All_Desirable := False;

            --  Generic case with no object file available

            if At_Eol then
               Withs.Table (Withs.Last).Sfile := No_File;
               Withs.Table (Withs.Last).Afile := No_File;

            --  Normal case

            else
               Withs.Table (Withs.Last).Sfile := Get_Name (Lower => True);
               Withs.Table (Withs.Last).Afile := Get_Name;

               --  Scan out possible E, EA, and NE parameters

               while not At_Eol loop
                  Skip_Space;

                  if Nextc = 'E' then
                     P := P + 1;

                     if At_End_Of_Field then
                        Withs.Table (Withs.Last).Elaborate := True;

                     elsif Nextc = 'A' then
                        P := P + 1;
                        Check_At_End_Of_Field;
                        Withs.Table (Withs.Last).Elaborate_All := True;

                     else
                        Checkc ('D');
                        Check_At_End_Of_Field;

                        --  Store ED indication unless ignore required

                        if not Ignore_ED then
                           Withs.Table (Withs.Last).Elab_All_Desirable := True;
                        end if;
                     end if;
                  end if;
               end loop;
            end if;

            Skip_Eol;
            C := Getc;

         end loop With_Loop;

         Units.Table (Units.Last).Last_With := Withs.Last;
         Units.Table (Units.Last).Last_Arg  := Args.Last;

      end loop Unit_Loop;

      --  End loop through units for one ALI file

      ALIs.Table (Id).Last_Unit := Units.Last;
      ALIs.Table (Id).Sfile := Units.Table (ALIs.Table (Id).First_Unit).Sfile;

      --  Set types of the units (there can be at most 2 of them)

      if ALIs.Table (Id).First_Unit /= ALIs.Table (Id).Last_Unit then
         Units.Table (ALIs.Table (Id).First_Unit).Utype := Is_Body;
         Units.Table (ALIs.Table (Id).Last_Unit).Utype  := Is_Spec;

      else
         --  Deal with body only and spec only cases, note that the reason we
         --  do our own checking of the name (rather than using Is_Body_Name)
         --  is that Uname drags in far too much compiler junk!

         Get_Name_String (Units.Table (Units.Last).Uname);

         if Name_Buffer (Name_Len) = 'b' then
            Units.Table (Units.Last).Utype := Is_Body_Only;
         else
            Units.Table (Units.Last).Utype := Is_Spec_Only;
         end if;
      end if;

      --  If there are linker options lines present, scan them

      while C = 'L' loop
         Checkc (' ');
         Skip_Space;
         Checkc ('"');

         Name_Len := 0;
         loop
            C := Getc;

            if C < Character'Val (16#20#)
              or else C > Character'Val (16#7E#)
            then
               Fatal_Error;

            elsif C = '{' then
               C := Character'Val (0);

               declare
                  V : Natural;

               begin
                  V := 0;
                  for J in 1 .. 2 loop
                     C := Getc;

                     if C in '0' .. '9' then
                        V := V * 16 +
                               Character'Pos (C) - Character'Pos ('0');

                     elsif C in 'A' .. 'F' then
                        V := V * 16 +
                               Character'Pos (C) - Character'Pos ('A') + 10;

                     else
                        Fatal_Error;
                     end if;
                  end loop;

                  Checkc ('}');

                  Add_Char_To_Name_Buffer (Character'Val (V));
               end;

            else
               if C = '"' then
                  exit when Nextc /= '"';
                  C := Getc;
               end if;

               Add_Char_To_Name_Buffer (C);
            end if;
         end loop;

         Add_Char_To_Name_Buffer (nul);

         Skip_Eol;
         C := Getc;

         Linker_Options.Increment_Last;

         Linker_Options.Table (Linker_Options.Last).Name
           := Name_Enter;

         Linker_Options.Table (Linker_Options.Last).Unit
           := ALIs.Table (Id).First_Unit;

         Linker_Options.Table (Linker_Options.Last).Internal_File
           := Is_Internal_File_Name (F);

         Linker_Options.Table (Linker_Options.Last).Original_Pos
           := Linker_Options.Last;

      end loop;

      --  Scan out external version references and put in hash table

      while C = 'E' loop
         Checkc (' ');
         Skip_Space;

         Name_Len := 0;
         Name_Len := 0;
         loop
            C := Getc;

            if C < ' ' then
               Fatal_Error;
            end if;

            exit when At_End_Of_Field;
            Add_Char_To_Name_Buffer (C);
         end loop;

         Version_Ref.Set (new String'(Name_Buffer (1 .. Name_Len)), True);
         Skip_Eol;
         C := Getc;
      end loop;

      --  Scan out source dependency lines for this ALI file

      ALIs.Table (Id).First_Sdep := Sdep.Last + 1;

      while C = 'D' loop
         Checkc (' ');
         Skip_Space;
         Sdep.Increment_Last;
         Sdep.Table (Sdep.Last).Sfile := Get_Name (Lower => True);
         Sdep.Table (Sdep.Last).Stamp := Get_Stamp;
         Sdep.Table (Sdep.Last).Dummy_Entry :=
           (Sdep.Table (Sdep.Last).Stamp = Dummy_Time_Stamp);

         --  Acquire checksum value

         Skip_Space;

         declare
            Ctr : Natural;
            Chk : Word;

         begin
            Ctr := 0;
            Chk := 0;

            loop
               exit when At_Eol or else Ctr = 8;

               if Nextc in '0' .. '9' then
                  Chk := Chk * 16 +
                           Character'Pos (Nextc) - Character'Pos ('0');

               elsif Nextc in 'a' .. 'f' then
                  Chk := Chk * 16 +
                           Character'Pos (Nextc) - Character'Pos ('a') + 10;

               else
                  exit;
               end if;

               Ctr := Ctr + 1;
               P := P + 1;
            end loop;

            if Ctr = 8 and then At_End_Of_Field then
               Sdep.Table (Sdep.Last).Checksum := Chk;
            else
               Fatal_Error;
            end if;
         end;

         --  Acquire subunit and reference file name entries

         Sdep.Table (Sdep.Last).Subunit_Name := No_Name;
         Sdep.Table (Sdep.Last).Rfile        := Sdep.Table (Sdep.Last).Sfile;
         Sdep.Table (Sdep.Last).Start_Line   := 1;

         if not At_Eol then
            Skip_Space;

            --  Here for subunit name

            if Nextc not in '0' .. '9' then
               Name_Len := 0;

               while not At_End_Of_Field loop
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Getc;
               end loop;

               Sdep.Table (Sdep.Last).Subunit_Name := Name_Enter;
               Skip_Space;
            end if;

            --  Here for reference file name entry

            if Nextc in '0' .. '9' then
               Sdep.Table (Sdep.Last).Start_Line := Get_Nat;
               Checkc (':');

               Name_Len := 0;

               while not At_End_Of_Field loop
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Getc;
               end loop;

               Sdep.Table (Sdep.Last).Rfile := Name_Enter;
            end if;
         end if;

         Skip_Eol;
         C := Getc;
      end loop;

      ALIs.Table (Id).Last_Sdep := Sdep.Last;

      --  Loop through Xref sections (skip loop if not reading xref stuff)

      while Read_Xref and then C = 'X' loop

         --  Make new entry in section table

         Xref_Section.Increment_Last;

         Read_Refs_For_One_File : declare
            XS : Xref_Section_Record renames
                   Xref_Section.Table (Xref_Section.Last);

            Current_File_Num : Sdep_Id;
            --  Keeps track of the current file number (changed by nn|)

         begin
            XS.File_Num     := Sdep_Id (Get_Nat + Nat (First_Sdep_Entry) - 1);
            XS.File_Name    := Get_Name;
            XS.First_Entity := Xref_Entity.Last + 1;

            Current_File_Num := XS.File_Num;

            Skip_Eol;
            C := Nextc;

            --  Loop through Xref entities

            while C /= 'X' and then C /= EOF loop
               Xref_Entity.Increment_Last;

               Read_Refs_For_One_Entity : declare

                  XE : Xref_Entity_Record renames
                         Xref_Entity.Table (Xref_Entity.Last);

                  N : Nat;

                  procedure Read_Instantiation_Reference;
                  --  Acquire instantiation reference. Caller has checked
                  --  that current character is '[' and on return the cursor
                  --  is skipped past the corresponding closing ']'.

                  ----------------------------------
                  -- Read_Instantiation_Reference --
                  ----------------------------------

                  procedure Read_Instantiation_Reference is
                  begin
                     Xref.Increment_Last;

                     declare
                        XR : Xref_Record renames Xref.Table (Xref.Last);

                     begin
                        P := P + 1; -- skip [
                        N := Get_Nat;

                        if Nextc = '|' then
                           XR.File_Num :=
                             Sdep_Id (N + Nat (First_Sdep_Entry) - 1);
                           Current_File_Num := XR.File_Num;
                           P := P + 1;
                           N := Get_Nat;

                        else
                           XR.File_Num := Current_File_Num;
                        end if;

                        XR.Line  := N;
                        XR.Rtype := ' ';
                        XR.Col   := 0;

                        --  Recursive call for next reference

                        if Nextc = '[' then
                           pragma Warnings (Off); -- kill recursion warning
                           Read_Instantiation_Reference;
                           pragma Warnings (On);
                        end if;

                        --  Skip closing bracket after recursive call

                        P := P + 1;
                     end;
                  end Read_Instantiation_Reference;

               --  Start of processing for Read_Refs_For_One_Entity

               begin
                  XE.Line   := Get_Nat;
                  XE.Etype  := Getc;
                  XE.Col    := Get_Nat;
                  XE.Lib    := (Getc = '*');
                  XE.Entity := Get_Name;

                  --  Renaming reference is present

                  if Nextc = '=' then
                     P := P + 1;
                     XE.Rref_Line := Get_Nat;

                     if Getc /= ':' then
                        Fatal_Error;
                     end if;

                     XE.Rref_Col := Get_Nat;

                  --  No renaming reference present

                  else
                     XE.Rref_Line := 0;
                     XE.Rref_Col  := 0;
                  end if;

                  Skip_Space;

                  --  See if type reference present

                  case Nextc is
                     when '<'    => XE.Tref := Tref_Derived;
                     when '('    => XE.Tref := Tref_Access;
                     when '{'    => XE.Tref := Tref_Type;
                     when others => XE.Tref := Tref_None;
                  end case;

                  --  Case of typeref field present

                  if XE.Tref /= Tref_None then
                     P := P + 1; -- skip opening bracket

                     if Nextc in 'a' .. 'z' then
                        XE.Tref_File_Num        := No_Sdep_Id;
                        XE.Tref_Line            := 0;
                        XE.Tref_Type            := ' ';
                        XE.Tref_Col             := 0;
                        XE.Tref_Standard_Entity := Get_Name;

                     else
                        N := Get_Nat;

                        if Nextc = '|' then
                           XE.Tref_File_Num :=
                             Sdep_Id (N + Nat (First_Sdep_Entry) - 1);
                           P := P + 1;
                           N := Get_Nat;

                        else
                           XE.Tref_File_Num := Current_File_Num;
                        end if;

                        XE.Tref_Line            := N;
                        XE.Tref_Type            := Getc;
                        XE.Tref_Col             := Get_Nat;
                        XE.Tref_Standard_Entity := No_Name;
                     end if;

                     P := P + 1; -- skip closing bracket
                     Skip_Space;

                  --  No typeref entry present

                  else
                     XE.Tref_File_Num        := No_Sdep_Id;
                     XE.Tref_Line            := 0;
                     XE.Tref_Type            := ' ';
                     XE.Tref_Col             := 0;
                     XE.Tref_Standard_Entity := No_Name;
                  end if;

                  XE.First_Xref := Xref.Last + 1;

                  --  Loop through cross-references for this entity

                  Current_File_Num := XS.File_Num;

                  loop
                     Skip_Space;

                     if At_Eol then
                        Skip_Eol;
                        exit when Nextc /= '.';
                        P := P + 1;
                     end if;

                     Xref.Increment_Last;

                     declare
                        XR : Xref_Record renames Xref.Table (Xref.Last);

                     begin
                        N := Get_Nat;

                        if Nextc = '|' then
                           XR.File_Num :=
                             Sdep_Id (N + Nat (First_Sdep_Entry) - 1);
                           Current_File_Num := XR.File_Num;
                           P := P + 1;
                           N := Get_Nat;

                        else
                           XR.File_Num := Current_File_Num;
                        end if;

                        XR.Line  := N;
                        XR.Rtype := Getc;
                        XR.Col   := Get_Nat;

                        if Nextc = '[' then
                           Read_Instantiation_Reference;
                        end if;
                     end;
                  end loop;

                  --  Record last cross-reference

                  XE.Last_Xref := Xref.Last;
                  C := Nextc;

               end Read_Refs_For_One_Entity;
            end loop;

            --  Record last entity

            XS.Last_Entity := Xref_Entity.Last;

         end Read_Refs_For_One_File;

         C := Getc;
      end loop;

      --  Here after dealing with xref sections

      if C /= EOF and then C /= 'X' then
         Fatal_Error;
      end if;

      return Id;

   exception
      when Bad_ALI_Format =>
         return No_ALI_Id;

   end Scan_ALI;

   ---------
   -- SEq --
   ---------

   function SEq (F1, F2 : String_Ptr) return Boolean is
   begin
      return F1.all = F2.all;
   end SEq;

   -----------
   -- SHash --
   -----------

   function SHash (S : String_Ptr) return Vindex is
      H : Word;

   begin
      H := 0;
      for J in S.all'Range loop
         H := H * 2 + Character'Pos (S (J));
      end loop;

      return Vindex (Vindex'First + Vindex (H mod Vindex'Range_Length));
   end SHash;

end ALI;
