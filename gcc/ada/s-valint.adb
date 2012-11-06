------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L _ I N T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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
with System.Val_Uns;        use System.Val_Uns;
with System.Val_Util;       use System.Val_Util;

package body System.Val_Int is

   ------------------
   -- Scan_Integer --
   ------------------

   function Scan_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer) return Integer
   is
      Uval : Unsigned;
      --  Unsigned result

      Minus : Boolean := False;
      --  Set to True if minus sign is present, otherwise to False

      Start : Positive;
      --  Saves location of first non-blank (not used in this case)

   begin
      Scan_Sign (Str, Ptr, Max, Minus, Start);

      if Str (Ptr.all) not in '0' .. '9' then
         Ptr.all := Start;
         Bad_Value (Str);
      end if;

      Uval := Scan_Raw_Unsigned (Str, Ptr, Max);

      --  Deal with overflow cases, and also with maximum negative number

      if Uval > Unsigned (Integer'Last) then
         if Minus and then Uval = Unsigned (-(Integer'First)) then
            return Integer'First;
         else
            Bad_Value (Str);
         end if;

      --  Negative values

      elsif Minus then
         return -(Integer (Uval));

      --  Positive values

      else
         return Integer (Uval);
      end if;
   end Scan_Integer;

   -------------------
   -- Value_Integer --
   -------------------

   function Value_Integer (Str : String) return Integer is
      V : Integer;
      P : aliased Integer := Str'First;
   begin
      V := Scan_Integer (Str, P'Access, Str'Last);
      Scan_Trailing_Blanks (Str, P);
      return V;
   end Value_Integer;

end System.Val_Int;
