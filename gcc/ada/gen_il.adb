------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                G E N _ I L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2020-2021, Free Software Foundation, Inc.        --
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

package body Gen_IL is

   function Image (X : Root_Int) return String is
      Result : constant String := X'Img;
   begin
      if Result (1) = ' ' then
         return Result (2 .. Result'Last);
      else
         return Result;
      end if;
   end Image;

   procedure Capitalize (S : in out String) is
      Cap : Boolean := True;
   begin
      for X of S loop
         declare
            Old : constant Character := X;
         begin
            if Cap then
               X := To_Upper (X);
            else
               X := To_Lower (X);
            end if;

            Cap := not (Is_Letter (Old) or else Is_Digit (Old));
         end;
      end loop;
   end Capitalize;

   function Capitalize (S : String) return String is
   begin
      return Result : String (S'Range) := S do
         Capitalize (Result);
      end return;
   end Capitalize;

end Gen_IL;
