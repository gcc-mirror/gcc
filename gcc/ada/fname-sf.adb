------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F N A M E . S F                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Casing;   use Casing;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with SFN_Scan; use SFN_Scan;
with Namet;    use Namet;
with Osint;    use Osint;
with Types;    use Types;

with Unchecked_Conversion;

package body Fname.SF is

   subtype Big_String is String (Positive);
   type Big_String_Ptr is access all Big_String;

   function To_Big_String_Ptr is new Unchecked_Conversion
     (Source_Buffer_Ptr, Big_String_Ptr);

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
      Src : Source_Buffer_Ptr;
      Hi  : Source_Ptr;
      BS  : Big_String_Ptr;
      SP  : String_Ptr;

   begin
      Name_Buffer (1 .. 8) := "gnat.adc";
      Name_Len := 8;
      Read_Source_File (Name_Enter, 0, Hi, Src);

      if Src /= null then
         BS := To_Big_String_Ptr (Src);
         SP := BS (1 .. Natural (Hi))'Unrestricted_Access;
         Scan_SFN_Pragmas
           (SP.all,
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
