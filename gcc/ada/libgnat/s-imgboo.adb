------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      S Y S T E M . I M G _ B O O L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 1992-2019, Free Software Foundation, Inc.        --
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

package body System.Img_Bool is

   -------------------
   -- Image_Boolean --
   -------------------

   procedure Image_Boolean
     (V : Boolean;
      S : in out String;
      P : out Natural)
   is
      pragma Assert (S'First = 1);
   begin
      if V then
         S (1 .. 4) := "TRUE";
         P := 4;
      else
         S (1 .. 5) := "FALSE";
         P := 5;
      end if;
   end Image_Boolean;

end System.Img_Bool;
