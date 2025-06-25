------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . V A L U E _ I                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with System.Val_Util; use System.Val_Util;

package body System.Value_I is

   ------------------
   -- Scan_Integer --
   ------------------

   procedure Scan_Integer
     (Str : String;
      Ptr : not null access Integer;
      Max : Integer;
      Res : out Int)
   is
      Uval : Uns;
      --  Unsigned result

      Minus : Boolean;
      --  Set to True if minus sign is present, otherwise to False

      Unused_Start : Positive;
      --  Saves location of first non-blank (not used in this case)

   begin
      Scan_Sign (Str, Ptr, Max, Minus, Unused_Start);

      if Str (Ptr.all) not in '0' .. '9' then
         Ptr.all := Unused_Start;
         Bad_Value (Str);
      end if;

      Scan_Raw_Unsigned (Str, Ptr, Max, Uval);

      --  Deal with overflow cases, and also with largest negative number

      if Uval > Uns (Int'Last) then
         if Minus and then Uval = Uns (Int'Last) + 1 then
            Res := Int'First;
         else
            Bad_Value (Str);
         end if;

      --  Negative values

      elsif Minus then
         Res := -(Int (Uval));

      --  Positive values

      else
         Res := Int (Uval);
      end if;
   end Scan_Integer;

   -------------------
   -- Value_Integer --
   -------------------

   function Value_Integer (Str : String) return Int is
   begin
      --  We have to special case Str'Last = Positive'Last because the normal
      --  circuit ends up setting P to Str'Last + 1 which is out of bounds. We
      --  deal with this by converting to a subtype which fixes the bounds.

      if Str'Last = Positive'Last then
         declare
            subtype NT is String (1 .. Str'Length);
         begin
            return Value_Integer (NT (Str));
         end;

      --  Normal case where Str'Last < Positive'Last

      else
         declare
            V : Int;
            P : aliased Integer := Str'First;
         begin
            declare
               P_Acc : constant not null access Integer := P'Access;
            begin
               Scan_Integer (Str, P_Acc, Str'Last, V);
            end;

            Scan_Trailing_Blanks (Str, P);
            return V;
         end;
      end if;
   end Value_Integer;

end System.Value_I;
