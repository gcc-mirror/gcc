------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                B U T I L                                 --
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

with Hostparm; use Hostparm;
with Namet;    use Namet;
with Output;   use Output;

package body Butil is

   ----------------------
   -- Is_Internal_Unit --
   ----------------------

   --  Note: the reason we do not use the Fname package for this function
   --  is that it would drag too much junk into the binder.

   function Is_Internal_Unit return Boolean is
   begin
      return Is_Predefined_Unit
        or else (Name_Len > 4
                   and then (Name_Buffer (1 .. 5) = "gnat%"
                               or else
                             Name_Buffer (1 .. 5) = "gnat."))
        or else
          (OpenVMS
             and then Name_Len > 3
             and then (Name_Buffer (1 .. 4) = "dec%"
                         or else
                       Name_Buffer (1 .. 4) = "dec."));

   end Is_Internal_Unit;

   ------------------------
   -- Is_Predefined_Unit --
   ------------------------

   --  Note: the reason we do not use the Fname package for this function
   --  is that it would drag too much junk into the binder.

   function Is_Predefined_Unit return Boolean is
   begin
      return    (Name_Len >  3
                  and then Name_Buffer (1 ..  4) = "ada.")

        or else (Name_Len >  6
                  and then Name_Buffer (1 ..  7) = "system.")

        or else (Name_Len > 10
                   and then Name_Buffer (1 .. 11) = "interfaces.")

        or else (Name_Len >  3
                   and then Name_Buffer (1 ..  4) = "ada%")

        or else (Name_Len >  8
                   and then Name_Buffer (1 ..  9) = "calendar%")

        or else (Name_Len >  9
                   and then Name_Buffer (1 .. 10) = "direct_io%")

        or else (Name_Len > 10
                   and then Name_Buffer (1 .. 11) = "interfaces%")

        or else (Name_Len > 13
                   and then Name_Buffer (1 .. 14) = "io_exceptions%")

        or else (Name_Len > 12
                   and then Name_Buffer (1 .. 13) = "machine_code%")

        or else (Name_Len > 13
                   and then Name_Buffer (1 .. 14) = "sequential_io%")

        or else (Name_Len >  6
                   and then Name_Buffer (1 ..  7) = "system%")

        or else (Name_Len >  7
                   and then Name_Buffer (1 ..  8) = "text_io%")

        or else (Name_Len > 20
                   and then Name_Buffer (1 .. 21) = "unchecked_conversion%")

        or else (Name_Len > 22
                   and then Name_Buffer (1 .. 23) = "unchecked_deallocation%")

        or else (Name_Len > 4
                   and then Name_Buffer (1 .. 5) = "gnat%")

        or else (Name_Len > 4
                   and then Name_Buffer (1 .. 5) = "gnat.");
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

         for I in 1 .. Min_Length loop
            if U1_Name (I) > Name_Buffer (I) then
               return False;
            elsif U1_Name (I) < Name_Buffer (I) then
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
