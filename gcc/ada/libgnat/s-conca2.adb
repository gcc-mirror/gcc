------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                      S Y S T E M . C O N C A T _ 2                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008-2019, Free Software Foundation, Inc.       --
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

pragma Compiler_Unit_Warning;

package body System.Concat_2 is

   pragma Suppress (All_Checks);

   ------------------
   -- Str_Concat_2 --
   ------------------

   procedure Str_Concat_2 (R : out String; S1, S2 : String) is
      F, L : Natural;

   begin
      F := R'First;
      L := F + S1'Length - 1;
      R (F .. L) := S1;

      F := L + 1;
      L := R'Last;
      R (F .. L) := S2;
   end Str_Concat_2;

   -------------------------
   -- Str_Concat_Bounds_2 --
   -------------------------

   procedure Str_Concat_Bounds_2
     (Lo, Hi : out Natural;
      S1, S2 : String)
   is
   begin
      if S1 = "" then
         Lo := S2'First;
         Hi := S2'Last;
      else
         Lo := S1'First;
         Hi := S1'Last + S2'Length;
      end if;
   end Str_Concat_Bounds_2;

end System.Concat_2;
