------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . C A S E _ U T I L                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1995-2005 AdaCore                      --
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

package body System.Case_Util is

   --------------
   -- To_Lower --
   --------------

   function To_Lower (A : Character) return Character is
      A_Val : constant Natural := Character'Pos (A);

   begin
      if A in 'A' .. 'Z'
        or else A_Val in 16#C0# .. 16#D6#
        or else A_Val in 16#D8# .. 16#DE#
      then
         return Character'Val (A_Val + 16#20#);
      else
         return A;
      end if;
   end To_Lower;

   procedure To_Lower (A : in out String) is
   begin
      for J in A'Range loop
         A (J) := To_Lower (A (J));
      end loop;
   end To_Lower;

   --------------
   -- To_Mixed --
   --------------

   procedure To_Mixed (A : in out String) is
      Ucase : Boolean := True;

   begin
      for J in A'Range loop
         if Ucase then
            A (J) := To_Upper (A (J));
         else
            A (J) := To_Lower (A (J));
         end if;

         Ucase := A (J) = '_';
      end loop;
   end To_Mixed;

   --------------
   -- To_Upper --
   --------------

   function To_Upper (A : Character) return Character is
      A_Val : constant Natural := Character'Pos (A);

   begin
      if A in 'a' .. 'z'
        or else A_Val in 16#E0# .. 16#F6#
        or else A_Val in 16#F8# .. 16#FE#
      then
         return Character'Val (A_Val - 16#20#);
      else
         return A;
      end if;
   end To_Upper;

   procedure To_Upper (A : in out String) is
   begin
      for J in A'Range loop
         A (J) := To_Upper (A (J));
      end loop;
   end To_Upper;

end System.Case_Util;
