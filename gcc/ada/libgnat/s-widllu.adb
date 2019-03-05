------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W I D _ L L U                        --
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
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Unsigned_Types; use System.Unsigned_Types;

package body System.Wid_LLU is

   ------------------------------
   -- Width_Long_Long_Unsigned --
   ------------------------------

   function Width_Long_Long_Unsigned
     (Lo, Hi : Long_Long_Unsigned)
      return   Natural
   is
      W : Natural;
      T : Long_Long_Unsigned;

   begin
      if Lo > Hi then
         return 0;

      else
         --  Minimum value is 2, one for sign, one for digit

         W := 2;

         --  Get max of absolute values, but avoid bomb if we have the maximum
         --  negative number (note that First + 1 has same digits as First)

         T := Long_Long_Unsigned'Max (Lo, Hi);

         --  Increase value if more digits required

         while T >= 10 loop
            T := T / 10;
            W := W + 1;
         end loop;

         return W;
      end if;

   end Width_Long_Long_Unsigned;

end System.Wid_LLU;
