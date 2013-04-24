------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G E T _ S P A R K _ X R E F S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2011-2013, Free Software Foundation, Inc.         --
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

with SPARK_Xrefs; use SPARK_Xrefs;
with Types;       use Types;

with Ada.IO_Exceptions; use Ada.IO_Exceptions;

procedure Get_SPARK_Xrefs is
   C : Character;

   use ASCII;
   --  For CR/LF

   Cur_File : Nat;
   --  Dependency number for the current file

   Cur_Scope : Nat;
   --  Scope number for the current scope entity

   Cur_File_Idx : File_Index;
   --  Index in SPARK_File_Table of the current file

   Cur_Scope_Idx : Scope_Index;
   --  Index in SPARK_Scope_Table of the current scope

   Name_Str : String (1 .. 32768);
   Name_Len : Natural := 0;
   --  Local string used to store name of File/entity scanned as
   --  Name_Str (1 .. Name_Len).

   File_Name : String_Ptr;
   Unit_File_Name : String_Ptr;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function At_EOL return Boolean;
   --  Skips any spaces, then checks if at the end of a line. If so, returns
   --  True (but does not skip the EOL sequence). If not, then returns False.

   procedure Check (C : Character);
   --  Checks that file is positioned at given character, and if so skips past
   --  it, If not, raises Data_Error.

   function Get_Nat return Nat;
   --  On entry the file is positioned to a digit. On return, the file is
   --  positioned past the last digit, and the returned result is the decimal
   --  value read. Data_Error is raised for overflow (value greater than
   --  Int'Last), or if the initial character is not a digit.

   procedure Get_Name;
   --  On entry the file is positioned to a name. On return, the file is
   --  positioned past the last character, and the name scanned is returned
   --  in Name_Str (1 .. Name_Len).

   procedure Skip_EOL;
   --  Called with the current character about to be read being LF or CR. Skips
   --  past CR/LF characters until either a non-CR/LF character is found, or
   --  the end of file is encountered.

   procedure Skip_Spaces;
   --  Skips zero or more spaces at the current position, leaving the file
   --  positioned at the first non-blank character (or Types.EOF).

   ------------
   -- At_EOL --
   ------------

   function At_EOL return Boolean is
   begin
      Skip_Spaces;
      return Nextc = CR or else Nextc = LF;
   end At_EOL;

   -----------
   -- Check --
   -----------

   procedure Check (C : Character) is
   begin
      if Nextc = C then
         Skipc;
      else
         raise Data_Error;
      end if;
   end Check;

   -------------
   -- Get_Nat --
   -------------

   function Get_Nat return Nat is
      Val : Nat;
      C   : Character;

   begin
      C := Nextc;
      Val := 0;

      if C not in '0' .. '9' then
         raise Data_Error;
      end if;

      --  Loop to read digits of integer value

      loop
         declare
            pragma Unsuppress (Overflow_Check);
         begin
            Val := Val * 10 + (Character'Pos (C) - Character'Pos ('0'));
         end;

         Skipc;
         C := Nextc;

         exit when C not in '0' .. '9';
      end loop;

      return Val;

   exception
      when Constraint_Error =>
         raise Data_Error;
   end Get_Nat;

   --------------
   -- Get_Name --
   --------------

   procedure Get_Name is
      N : Integer;

   begin
      N := 0;
      while Nextc > ' ' loop
         N := N + 1;
         Name_Str (N) := Getc;
      end loop;

      Name_Len := N;
   end Get_Name;

   --------------
   -- Skip_EOL --
   --------------

   procedure Skip_EOL is
      C : Character;

   begin
      loop
         Skipc;
         C := Nextc;
         exit when C /= LF and then C /= CR;

         if C = ' ' then
            Skip_Spaces;
            C := Nextc;
            exit when C /= LF and then C /= CR;
         end if;
      end loop;
   end Skip_EOL;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces is
   begin
      while Nextc = ' ' loop
         Skipc;
      end loop;
   end Skip_Spaces;

--  Start of processing for Get_SPARK_Xrefs

begin
   Initialize_SPARK_Tables;

   Cur_File      := 0;
   Cur_Scope     := 0;
   Cur_File_Idx  := 1;
   Cur_Scope_Idx := 0;

   --  Loop through lines of SPARK cross-reference information

   while Nextc = 'F' loop
      Skipc;

      C := Getc;

      --  Make sure first line is a File line

      if SPARK_File_Table.Last = 0 and then C /= 'D' then
         raise Data_Error;
      end if;

      --  Otherwise dispatch on type of line

      case C is

         --  Header entry for scope section

         when 'D' =>

            --  Complete previous entry if any

            if SPARK_File_Table.Last /= 0 then
               SPARK_File_Table.Table (SPARK_File_Table.Last).To_Scope :=
                 SPARK_Scope_Table.Last;
            end if;

            --  Scan out dependency number and file name

            Skip_Spaces;
            Cur_File := Get_Nat;
            Skip_Spaces;

            Get_Name;
            File_Name := new String'(Name_Str (1 .. Name_Len));
            Skip_Spaces;

            --  Scan out unit file name when present (for subunits)

            if Nextc = '-' then
               Skipc;
               Check ('>');
               Skip_Spaces;
               Get_Name;
               Unit_File_Name := new String'(Name_Str (1 .. Name_Len));

            else
               Unit_File_Name := null;
            end if;

            --  Make new File table entry (will fill in To_Scope later)

            SPARK_File_Table.Append (
              (File_Name      => File_Name,
               Unit_File_Name => Unit_File_Name,
               File_Num       => Cur_File,
               From_Scope     => SPARK_Scope_Table.Last + 1,
               To_Scope       => 0));

            --  Initialize counter for scopes

            Cur_Scope := 1;

         --  Scope entry

         when 'S' =>
            declare
               Spec_File  : Nat;
               Spec_Scope : Nat;
               Scope      : Nat;
               Line       : Nat;
               Col        : Nat;
               Typ        : Character;

            begin
               --  Scan out location

               Skip_Spaces;
               Check ('.');
               Scope := Get_Nat;
               Check (' ');
               Line  := Get_Nat;
               Typ   := Getc;
               Col   := Get_Nat;

               pragma Assert (Scope = Cur_Scope);
               pragma Assert         (Typ = 'K'
                              or else Typ = 'V'
                              or else Typ = 'U');

               --  Scan out scope entity name

               Skip_Spaces;
               Get_Name;
               Skip_Spaces;

               if Nextc = '-' then
                  Skipc;
                  Check ('>');
                  Skip_Spaces;
                  Spec_File := Get_Nat;
                  Check ('.');
                  Spec_Scope := Get_Nat;

               else
                  Spec_File  := 0;
                  Spec_Scope := 0;
               end if;

               --  Make new scope table entry (will fill in From_Xref and
               --  To_Xref later). Initial range (From_Xref .. To_Xref) is
               --  empty for scopes without entities.

               SPARK_Scope_Table.Append (
                 (Scope_Entity   => Empty,
                  Scope_Name     => new String'(Name_Str (1 .. Name_Len)),
                  File_Num       => Cur_File,
                  Scope_Num      => Cur_Scope,
                  Spec_File_Num  => Spec_File,
                  Spec_Scope_Num => Spec_Scope,
                  Line           => Line,
                  Stype          => Typ,
                  Col            => Col,
                  From_Xref      => 1,
                  To_Xref        => 0));
            end;

            --  Update counter for scopes

            Cur_Scope := Cur_Scope + 1;

         --  Header entry for cross-ref section

         when 'X' =>

            --  Scan out dependency number and file name (ignored)

            Skip_Spaces;
            Cur_File := Get_Nat;
            Skip_Spaces;
            Get_Name;

            --  Update component From_Xref of current file if first reference
            --  in this file.

            while SPARK_File_Table.Table (Cur_File_Idx).File_Num /= Cur_File
            loop
               Cur_File_Idx := Cur_File_Idx + 1;
            end loop;

            --  Scan out scope entity number and entity name (ignored)

            Skip_Spaces;
            Check ('.');
            Cur_Scope := Get_Nat;
            Skip_Spaces;
            Get_Name;

            --  Update component To_Xref of previous scope

            if Cur_Scope_Idx /= 0 then
               SPARK_Scope_Table.Table (Cur_Scope_Idx).To_Xref :=
                 SPARK_Xref_Table.Last;
            end if;

            --  Update component From_Xref of current scope

            Cur_Scope_Idx := SPARK_File_Table.Table (Cur_File_Idx).From_Scope;

            while SPARK_Scope_Table.Table (Cur_Scope_Idx).Scope_Num /=
              Cur_Scope
            loop
               Cur_Scope_Idx := Cur_Scope_Idx + 1;
            end loop;

            SPARK_Scope_Table.Table (Cur_Scope_Idx).From_Xref :=
              SPARK_Xref_Table.Last + 1;

         --  Cross reference entry

         when ' ' =>
            declare
               XR_Entity      : String_Ptr;
               XR_Entity_Line : Nat;
               XR_Entity_Col  : Nat;
               XR_Entity_Typ  : Character;

               XR_File : Nat;
               --  Keeps track of the current file (changed by nn|)

               XR_Scope : Nat;
               --  Keeps track of the current scope (changed by nn:)

            begin
               XR_File  := Cur_File;
               XR_Scope := Cur_Scope;

               XR_Entity_Line := Get_Nat;
               XR_Entity_Typ  := Getc;
               XR_Entity_Col  := Get_Nat;

               Skip_Spaces;
               Get_Name;
               XR_Entity := new String'(Name_Str (1 .. Name_Len));

               --  Initialize to scan items on one line

               Skip_Spaces;

               --  Loop through cross-references for this entity

               loop

                  declare
                     Line  : Nat;
                     Col   : Nat;
                     N     : Nat;
                     Rtype : Character;

                  begin
                     Skip_Spaces;

                     if At_EOL then
                        Skip_EOL;
                        exit when Nextc /= '.';
                        Skipc;
                        Skip_Spaces;
                     end if;

                     if Nextc = '.' then
                        Skipc;
                        XR_Scope := Get_Nat;
                        Check (':');

                     else
                        N := Get_Nat;

                        if Nextc = '|' then
                           XR_File := N;
                           Skipc;

                        else
                           Line  := N;
                           Rtype := Getc;
                           Col   := Get_Nat;

                           pragma Assert
                             (Rtype = 'r' or else
                              Rtype = 'm' or else
                              Rtype = 's');

                           SPARK_Xref_Table.Append (
                             (Entity_Name => XR_Entity,
                              Entity_Line => XR_Entity_Line,
                              Etype       => XR_Entity_Typ,
                              Entity_Col  => XR_Entity_Col,
                              File_Num    => XR_File,
                              Scope_Num   => XR_Scope,
                              Line        => Line,
                              Rtype       => Rtype,
                              Col         => Col));
                        end if;
                     end if;
                  end;
               end loop;
            end;

         --  No other SPARK lines are possible

         when others =>
            raise Data_Error;
      end case;

      --  For cross reference lines, the EOL character has been skipped already

      if C /= ' ' then
         Skip_EOL;
      end if;
   end loop;

   --  Here with all Xrefs stored, complete last entries in File/Scope tables

   if SPARK_File_Table.Last /= 0 then
      SPARK_File_Table.Table (SPARK_File_Table.Last).To_Scope :=
        SPARK_Scope_Table.Last;
   end if;

   if Cur_Scope_Idx /= 0 then
      SPARK_Scope_Table.Table (Cur_Scope_Idx).To_Xref := SPARK_Xref_Table.Last;
   end if;
end Get_SPARK_Xrefs;
