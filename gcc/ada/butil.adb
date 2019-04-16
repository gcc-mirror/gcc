------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B U T I L                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Output; use Output;

package body Butil is

   ----------------------
   -- Is_Internal_Unit --
   ----------------------

   --  Note: the reason we do not use the Fname package for this function
   --  is that it would drag too much junk into the binder.

   function Is_Internal_Unit return Boolean is
   begin
      return Is_Predefined_Unit
        or else (Name_Len > 4 and then (Name_Buffer (1 .. 5) = "gnat%"
                                          or else
                                        Name_Buffer (1 .. 5) = "gnat."));
   end Is_Internal_Unit;

   ------------------------
   -- Is_Predefined_Unit --
   ------------------------

   --  Note: the reason we do not use the Fname package for this function
   --  is that it would drag too much junk into the binder.

   function Is_Predefined_Unit return Boolean is
      L : Natural renames Name_Len;
      B : String  renames Name_Buffer;
   begin
      return    (L >  3 and then B (1 ..  4) = "ada.")
        or else (L >  6 and then B (1 ..  7) = "system.")
        or else (L > 10 and then B (1 .. 11) = "interfaces.")
        or else (L >  3 and then B (1 ..  4) = "ada%")
        or else (L >  8 and then B (1 ..  9) = "calendar%")
        or else (L >  9 and then B (1 .. 10) = "direct_io%")
        or else (L > 10 and then B (1 .. 11) = "interfaces%")
        or else (L > 13 and then B (1 .. 14) = "io_exceptions%")
        or else (L > 12 and then B (1 .. 13) = "machine_code%")
        or else (L > 13 and then B (1 .. 14) = "sequential_io%")
        or else (L >  6 and then B (1 ..  7) = "system%")
        or else (L >  7 and then B (1 ..  8) = "text_io%")
        or else (L > 20 and then B (1 .. 21) = "unchecked_conversion%")
        or else (L > 22 and then B (1 .. 23) = "unchecked_deallocation%")
        or else (L >  4 and then B (1 ..  5) = "gnat%")
        or else (L >  4 and then B (1 ..  5) = "gnat.");
   end Is_Predefined_Unit;

   ----------------
   -- Uname_Less --
   ----------------

   function Uname_Less (U1, U2 : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (U1);

      declare
         U1_Name : constant String (1 .. Name_Len) :=
                     Name_Buffer (1 .. Name_Len);
         Min_Length : Natural;

      begin
         Get_Name_String (U2);

         if Name_Len < U1_Name'Last then
            Min_Length := Name_Len;
         else
            Min_Length := U1_Name'Last;
         end if;

         for J in 1 .. Min_Length loop
            if U1_Name (J) > Name_Buffer (J) then
               return False;
            elsif U1_Name (J) < Name_Buffer (J) then
               return True;
            end if;
         end loop;

         return U1_Name'Last < Name_Len;
      end;
   end Uname_Less;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (U : Unit_Name_Type) is
   begin
      Get_Name_String (U);
      Write_Str (Name_Buffer (1 .. Name_Len - 2));

      if Name_Buffer (Name_Len) = 's' then
         Write_Str (" (spec)");
      else
         Write_Str (" (body)");
      end if;

      Name_Len := Name_Len + 5;
   end Write_Unit_Name;

end Butil;
