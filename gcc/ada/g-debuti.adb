------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 G N A T . D E B U G _ U T I L I T I E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1997-2005, AdaCore                     --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body GNAT.Debug_Utilities is

      H : constant array (0 .. 15) of Character := "0123456789ABCDEF";
      --  Table of hex digits

   -----------
   -- Image --
   -----------

   --  Address case

   function Image (A : Address) return Image_String is
      S : Image_String;
      P : Natural;
      N : Integer_Address;
      U : Natural := 0;

   begin
      S (S'Last) := '#';
      P := Address_Image_Length - 1;
      N := To_Integer (A);
      while P > 3 loop
         if U = 4 then
            S (P) := '_';
            P := P - 1;
            U := 1;

         else
            U := U + 1;
         end if;

         S (P) := H (Integer (N mod 16));
         P := P - 1;
         N := N / 16;
      end loop;

      S (1 .. 3) := "16#";
      return S;
   end Image;

   -----------
   -- Image --
   -----------

   --  String case

   function Image (S : String) return String is
      W : String (1 .. 2 * S'Length + 2);
      P : Positive := 1;

   begin
      W (1) := '"';

      for J in S'Range loop
         if S (J) = '"' then
            P := P + 1;
            W (P) := '"';
         end if;

         P := P + 1;
         W (P) := S (J);
      end loop;

      P := P + 1;
      W (P) := '"';
      return W (1 .. P);
   end Image;

   -------------
   -- Image_C --
   -------------

   function Image_C (A : Address) return Image_C_String is
      S : Image_C_String;
      N : Integer_Address := To_Integer (A);

   begin
      for P in reverse 3 .. S'Last loop
         S (P) := H (Integer (N mod 16));
         N := N / 16;
      end loop;

      S (1 .. 2) := "0x";
      return S;
   end Image_C;

   -----------
   -- Value --
   -----------

   function Value (S : String) return System.Address is
      Base : Integer_Address := 10;
      Res  : Integer_Address := 0;
      Last : Natural := S'Last;
      C    : Character;
      N    : Integer_Address;

   begin
      --  Skip final Ada 95 base character

      if S (Last) = '#' or else S (Last) = ':' then
         Last := Last - 1;
      end if;

      --  Loop through characters

      for J in S'First .. Last loop
         C := S (J);

         --  C format hex constant

         if C = 'x' then
            if Res /= 0 then
               raise Constraint_Error;
            end if;

            Base := 16;

         --  Ada form based literal

         elsif C = '#' or else C = ':' then
            Base := Res;
            Res  := 0;

         --  Ignore all underlines

         elsif C = '_' then
            null;

         --  Otherwise must have digit

         else
            if C in '0' .. '9' then
               N := Character'Pos (C) - Character'Pos ('0');
            elsif C in 'A' .. 'F' then
               N := Character'Pos (C) - (Character'Pos ('A') - 10);
            elsif C in 'a' .. 'f' then
               N := Character'Pos (C) - (Character'Pos ('a') - 10);
            else
               raise Constraint_Error;
            end if;

            if N >= Base then
               raise Constraint_Error;
            else
               Res := Res * Base + N;
            end if;
         end if;
      end loop;

      return To_Address (Res);
   end Value;

end GNAT.Debug_Utilities;
