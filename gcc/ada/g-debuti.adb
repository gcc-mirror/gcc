------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--                 G N A T . D E B U G _ U T I L I T I E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1997-1998 Ada Core Technologies, Inc.            --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body GNAT.Debug_Utilities is

   --------------------------
   -- Image (address case) --
   --------------------------

   function Image (A : Address) return String is
      S : String (1 .. Address_Image_Length);
      P : Natural := S'Last - 1;
      N : Integer_Address := To_Integer (A);
      U : Natural := 0;

      H : array (Integer range 0 .. 15) of Character := "0123456789ABCDEF";

   begin
      S (S'Last) := '#';

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

   -------------------------
   -- Image (string case) --
   -------------------------

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

   -----------
   -- Value --
   -----------

   function Value (S : String) return System.Address is
      N : constant Integer_Address := Integer_Address'Value (S);

   begin
      return To_Address (N);
   end Value;

end GNAT.Debug_Utilities;
