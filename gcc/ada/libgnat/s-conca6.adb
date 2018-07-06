------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                      S Y S T E M . C O N C A T _ 6                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2008-2018, Free Software Foundation, Inc.       --
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

with System.Concat_5;

package body System.Concat_6 is

   pragma Suppress (All_Checks);

   ------------------
   -- Str_Concat_6 --
   ------------------

   procedure Str_Concat_6 (R : out String; S1, S2, S3, S4, S5, S6 : String) is
      F, L : Natural;

   begin
      F := R'First;
      L := F + S1'Length - 1;
      R (F .. L) := S1;

      F := L + 1;
      L := F + S2'Length - 1;
      R (F .. L) := S2;

      F := L + 1;
      L := F + S3'Length - 1;
      R (F .. L) := S3;

      F := L + 1;
      L := F + S4'Length - 1;
      R (F .. L) := S4;

      F := L + 1;
      L := F + S5'Length - 1;
      R (F .. L) := S5;

      F := L + 1;
      L := R'Last;
      R (F .. L) := S6;
   end Str_Concat_6;

   -------------------------
   -- Str_Concat_Bounds_6 --
   -------------------------

   procedure Str_Concat_Bounds_6
     (Lo, Hi                 : out Natural;
      S1, S2, S3, S4, S5, S6 : String)
   is
   begin
      System.Concat_5.Str_Concat_Bounds_5 (Lo, Hi, S2, S3, S4, S5, S6);

      if S1 /= "" then
         Hi := S1'Last + Hi - Lo + 1;
         Lo := S1'First;
      end if;
   end Str_Concat_Bounds_6;

end System.Concat_6;
