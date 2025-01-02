------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F N A M E . S F                              --
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

with Casing;        use Casing;
with Fname.UF;      use Fname.UF;
with SFN_Scan;      use SFN_Scan;
with Osint;         use Osint;
with Types;         use Types;
with System.OS_Lib; use System.OS_Lib;

package body Fname.SF is

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Set_File_Name
     (Typ   : Character;
      U     : String;
      F     : String;
      Index : Natural);
   --  This is a transfer function that is called from Scan_SFN_Pragmas,
   --  and reformats its parameters appropriately for the version of
   --  Set_File_Name found in Fname.SF.

   procedure Set_File_Name_Pattern
     (Pat : String;
      Typ : Character;
      Dot : String;
      Cas : Character);
   --  This is a transfer function that is called from Scan_SFN_Pragmas,
   --  and reformats its parameters appropriately for the version of
   --  Set_File_Name_Pattern found in Fname.SF.

   -----------------------------------
   -- Read_Source_File_Name_Pragmas --
   -----------------------------------

   procedure Read_Source_File_Name_Pragmas is
      FD  : File_Descriptor;
      Src : Source_Buffer_Ptr;
      Hi  : Source_Ptr;

   begin
      Read_Source_File (Name_Enter ("gnat.adc"), 1, Hi, Src, FD);

      if not Null_Source_Buffer_Ptr (Src) then
         --  We need to strip off the trailing EOF that was added by
         --  Read_Source_File, because there might be another EOF in
         --  the file, and two in a row causes Scan_SFN_Pragmas to give
         --  errors.

         pragma Assert (Src (Hi) = EOF);
         Scan_SFN_Pragmas
           (String (Src (1 .. Hi - 1)),
            Set_File_Name'Access,
            Set_File_Name_Pattern'Access);
      end if;
   end Read_Source_File_Name_Pragmas;

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name
     (Typ   : Character;
      U     : String;
      F     : String;
      Index : Natural)
   is
      Unm : Unit_Name_Type;
      Fnm : File_Name_Type;
   begin
      Name_Buffer (1 .. U'Length) := U;
      Name_Len := U'Length;
      Set_Casing (All_Lower_Case);
      Name_Buffer (Name_Len + 1) := '%';
      Name_Buffer (Name_Len + 2) := Typ;
      Name_Len := Name_Len + 2;
      Unm := Name_Find;
      Name_Buffer (1 .. F'Length) := F;
      Name_Len := F'Length;
      Fnm := Name_Find;
      Fname.UF.Set_File_Name (Unm, Fnm, Nat (Index));
   end Set_File_Name;

   ---------------------------
   -- Set_File_Name_Pattern --
   ---------------------------

   procedure Set_File_Name_Pattern
     (Pat : String;
      Typ : Character;
      Dot : String;
      Cas : Character)
   is
      Ctyp : Casing_Type;
      Patp : constant String_Ptr := new String'(Pat);
      Dotp : constant String_Ptr := new String'(Dot);

   begin
      if Cas = 'l' then
         Ctyp := All_Lower_Case;
      elsif Cas = 'u' then
         Ctyp := All_Upper_Case;
      else -- Cas = 'm'
         Ctyp := Mixed_Case;
      end if;

      Fname.UF.Set_File_Name_Pattern (Patp, Typ, Dotp, Ctyp);
   end Set_File_Name_Pattern;

end Fname.SF;
