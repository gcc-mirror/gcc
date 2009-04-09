------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . S T R I N G _ O P S _ C O N C A T _ 4            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

package body System.String_Ops_Concat_4 is

   ------------------
   -- Str_Concat_4 --
   ------------------

   function Str_Concat_4 (S1, S2, S3, S4 : String) return String is
   begin
      if S1'Length = 0 then
         return S2 & S3 & S4;

      else
         declare
            L12 : constant Natural := S1'Length + S2'Length;
            L13 : constant Natural := L12 + S3'Length;
            L14 : constant Natural := L13 + S4'Length;
            R : String (S1'First .. S1'First + L14 - 1);

         begin
            R (S1'First       .. S1'Last)            := S1;
            R (S1'Last + 1    .. S1'First + L12 - 1) := S2;
            R (S1'First + L12 .. S1'First + L13 - 1) := S3;
            R (S1'First + L13 .. R'Last)             := S4;
            return R;
         end;
      end if;
   end Str_Concat_4;

end System.String_Ops_Concat_4;
