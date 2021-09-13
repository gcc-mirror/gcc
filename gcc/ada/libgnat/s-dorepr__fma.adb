------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . D O U B L E _ R E A L . P R O D U C T           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2021, Free Software Foundation, Inc.            --
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

--  This is the version of the separate package body for targets with an FMA

separate (System.Double_Real)

package body Product is

   function Fused_Multiply_Add (A, B, C : Num) return Num;
   --  Return the result of A * B + C without intermediate rounding

   ------------------------
   -- Fused_Multiply_Add --
   ------------------------

   function Fused_Multiply_Add (A, B, C : Num) return Num is
   begin
      case Num'Size is
         when 32 =>
            declare
               function Do_FMA (A, B, C : Num) return Num;
               pragma Import (Intrinsic, Do_FMA, "__builtin_fmaf");

            begin
               return Do_FMA (A, B, C);
            end;

         when 64 =>
            declare
               function Do_FMA (A, B, C : Num) return Num;
               pragma Import (Intrinsic, Do_FMA, "__builtin_fma");

            begin
               return Do_FMA (A, B, C);
            end;

         when others =>
            raise Program_Error;
      end case;
   end Fused_Multiply_Add;

   --------------
   -- Two_Prod --
   --------------

   function Two_Prod (A, B : Num) return Double_T is
      P : constant Num := A * B;

      E : Num;

   begin
      if Is_Infinity (P) or else Is_Zero (P) then
         return (P, 0.0);

      else
         E := Fused_Multiply_Add (A, B, -P);

         return (P, E);
      end if;
   end Two_Prod;

   -------------
   -- Two_Sqr --
   -------------

   function Two_Sqr (A : Num) return Double_T is (Two_Prod (A, A));

end Product;
