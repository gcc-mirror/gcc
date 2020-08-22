------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                    S Y S T E M . S T R I N G _ O P S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  NOTE: This package is obsolescent. It is no longer used by the compiler
--  which now generates concatenation inline. It is retained only because
--  it may be used during bootstrapping using old versions of the compiler.

pragma Compiler_Unit_Warning;

package body System.String_Ops is

   ----------------
   -- Str_Concat --
   ----------------

   function Str_Concat (X, Y : String) return String is
   begin
      if X'Length = 0 then
         return Y;

      else
         declare
            L : constant Natural := X'Length + Y'Length;
            R : String (X'First .. X'First + L - 1);

         begin
            R (X'Range) := X;
            R (X'First + X'Length .. R'Last) := Y;
            return R;
         end;
      end if;
   end Str_Concat;

   -------------------
   -- Str_Concat_CC --
   -------------------

   function Str_Concat_CC (X, Y : Character) return String is
      R : String (1 .. 2);

   begin
      R (1) := X;
      R (2) := Y;
      return R;
   end Str_Concat_CC;

   -------------------
   -- Str_Concat_CS --
   -------------------

   function Str_Concat_CS (X : Character; Y : String) return String is
      R : String (1 .. Y'Length + 1);

   begin
      R (1) := X;
      R (2 .. R'Last) := Y;
      return R;
   end Str_Concat_CS;

   -------------------
   -- Str_Concat_SC --
   -------------------

   function Str_Concat_SC (X : String; Y : Character) return String is
   begin
      if X'Length = 0 then
         return (1 => Y);

      else
         declare
            R : String (X'First .. X'Last + 1);

         begin
            R (X'Range) := X;
            R (R'Last) := Y;
            return R;
         end;
      end if;
   end Str_Concat_SC;

end System.String_Ops;
