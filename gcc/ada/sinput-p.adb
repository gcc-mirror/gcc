------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S I N P U T . P                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Prj.Err;
with Sinput.C;

with System;

package body Sinput.P is

   First : Boolean := True;
   --  Flag used when Load_Project_File is called the first time,
   --  to set Main_Source_File.
   --  The flag is reset to False at the first call to Load_Project_File.
   --  Calling Reset_First sets it back to True.

   procedure Free is new Ada.Unchecked_Deallocation
     (Lines_Table_Type, Lines_Table_Ptr);

   procedure Free is new Ada.Unchecked_Deallocation
     (Logical_Lines_Table_Type, Logical_Lines_Table_Ptr);

   -----------------------------
   -- Clear_Source_File_Table --
   -----------------------------

   procedure Clear_Source_File_Table is
      use System;

   begin
      for X in 1 .. Source_File.Last loop
         declare
            S  : Source_File_Record renames Source_File.Table (X);
            Lo : constant Source_Ptr := S.Source_First;
            Hi : constant Source_Ptr := S.Source_Last;
            subtype Actual_Source_Buffer is Source_Buffer (Lo .. Hi);
            --  Physical buffer allocated

            type Actual_Source_Ptr is access Actual_Source_Buffer;
            --  This is the pointer type for the physical buffer allocated

            procedure Free is new Ada.Unchecked_Deallocation
              (Actual_Source_Buffer, Actual_Source_Ptr);

            pragma Suppress (All_Checks);

            pragma Warnings (Off);
            --  The following unchecked conversion is aliased safe, since it
            --  is not used to create improperly aliased pointer values.

            function To_Actual_Source_Ptr is new
              Ada.Unchecked_Conversion (Address, Actual_Source_Ptr);

            pragma Warnings (On);

            Actual_Ptr : Actual_Source_Ptr :=
                           To_Actual_Source_Ptr (S.Source_Text (Lo)'Address);

         begin
            Free (Actual_Ptr);
            Free (S.Lines_Table);
            Free (S.Logical_Lines_Table);
         end;
      end loop;

      Source_File.Free;
      Sinput.Initialize;
   end Clear_Source_File_Table;

   -----------------------
   -- Load_Project_File --
   -----------------------

   function Load_Project_File (Path : String) return Source_File_Index is
      X : Source_File_Index;

   begin
      X := Sinput.C.Load_File (Path);

      if First then
         Main_Source_File := X;
         First := False;
      end if;

      return X;
   end Load_Project_File;

   -----------------
   -- Reset_First --
   -----------------

   procedure Reset_First is
   begin
      First := True;
   end Reset_First;

   --------------------------------
   -- Restore_Project_Scan_State --
   --------------------------------

   procedure Restore_Project_Scan_State
     (Saved_State : Saved_Project_Scan_State)
   is
   begin
      Restore_Scan_State (Saved_State.Scan_State);
      Source              := Saved_State.Source;
      Current_Source_File := Saved_State.Current_Source_File;
   end Restore_Project_Scan_State;

   -----------------------------
   -- Save_Project_Scan_State --
   -----------------------------

   procedure Save_Project_Scan_State
     (Saved_State : out Saved_Project_Scan_State)
   is
   begin
      Save_Scan_State (Saved_State.Scan_State);
      Saved_State.Source              := Source;
      Saved_State.Current_Source_File := Current_Source_File;
   end Save_Project_Scan_State;

   ----------------------------
   -- Source_File_Is_Subunit --
   ----------------------------

   function Source_File_Is_Subunit (X : Source_File_Index) return Boolean is
   begin
      --  Nothing to do if X is no source file, so simply return False

      if X = No_Source_File then
         return False;
      end if;

      Prj.Err.Scanner.Initialize_Scanner (X);

      --  No error for special characters that are used for preprocessing

      Prj.Err.Scanner.Set_Special_Character ('#');
      Prj.Err.Scanner.Set_Special_Character ('$');

      --  We scan past junk to the first interesting compilation unit token, to
      --  see if it is SEPARATE. We ignore WITH keywords during this and also
      --  PRIVATE. The reason for ignoring PRIVATE is that it handles some
      --  error situations, and also to handle PRIVATE WITH in Ada 2005 mode.

      while Token = Tok_With
        or else Token = Tok_Private
        or else (Token not in Token_Class_Cunit and then Token /= Tok_EOF)
      loop
         Prj.Err.Scanner.Scan;
      end loop;

      Prj.Err.Scanner.Reset_Special_Characters;

      return Token = Tok_Separate;
   end Source_File_Is_Subunit;

end Sinput.P;
