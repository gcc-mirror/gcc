------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            S Y S T E M . D O U B L E _ R E A L . P R O D U C T           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2021-2025, Free Software Foundation, Inc.       --
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

--  This is the x86/FreeBSD version of the separate package body

with Interfaces; use Interfaces;

separate (System.Double_Real)

package body Product is

   procedure Split (N : Num; Hi : out Num; Lo : out Num);
   --  Compute high part and low part of N

   -----------
   -- Split --
   -----------

   --  We use a bit manipulation algorithm instead of Veltkamp's splitting
   --  because it is faster and has the property that the magnitude of the
   --  high part is never larger than that of the input number, which will
   --  avoid spurious overflows in the Two_Prod algorithm.

   --  See the recent paper by Claude-Pierre Jeannerod, Jean-Michel Muller
   --  and Paul Zimmermann: On various ways to split a floating-point number
   --  ARITH 2018 - 25th IEEE Symposium on Computer Arithmetic, Jun 2018,
   --  Amherst (MA), United States, pages 53-60.

   procedure Split (N : Num; Hi : out Num; Lo : out Num) is
      X : Num;

   begin
      --  Spill the input into the appropriate (maybe larger) bit container,
      --  mask out the low bits and reload the modified value.

      case Num'Machine_Mantissa is
         when 24 =>
            declare
               Rep32 : aliased Interfaces.Unsigned_32;
               Temp  : Num := N with Address => Rep32'Address;
               pragma Annotate (CodePeer, Modified, Rep32);

            begin
               --  Mask out the low 12 bits

               Rep32 := Rep32 and 16#FFFFF000#;

               X := Temp;
            end;

         when 53 =>
            declare
               Rep64 : aliased array (1 .. 2) of Interfaces.Unsigned_64;
               Temp  : Num := N with Address => Rep64'Address;
               pragma Annotate (CodePeer, Modified, Rep64);

            begin
               --  Mask out the low 27 bits

               Rep64 (1) := Rep64 (1) and 16#FFFFFFFFF8000000#;

               X := Temp;
            end;

         when 64 =>
            declare
               Rep80 : aliased array (1 .. 2) of Interfaces.Unsigned_64;
               Temp  : Num := N with Address => Rep80'Address;
               pragma Annotate (CodePeer, Modified, Rep80);

            begin
               --  Mask out the low 32 bits

               if System.Default_Bit_Order = High_Order_First then
                  Rep80 (1) := Rep80 (1) and 16#FFFFFFFFFFFF0000#;
                  Rep80 (2) := Rep80 (2) and 16#0000FFFFFFFFFFFF#;
               else
                  Rep80 (1) := Rep80 (1) and 16#FFFFFFFF00000000#;
               end if;

               X := Temp;
            end;

         when others =>
            raise Program_Error;
      end case;

      --  Deal with denormalized numbers

      if X = 0.0 then
         Hi := N;
         Lo := 0.0;
      else
         Hi := X;
         Lo := N - X;
      end if;
   end Split;

   --------------
   -- Two_Prod --
   --------------

   function Two_Prod (A, B : Num) return Double_T is
      P : constant Num := A * B;

      Ahi, Alo, Bhi, Blo, E : Num;

   begin
      if Is_Infinity (P) or else Is_Zero (P) then
         return (P, 0.0);

      else
         Split (A, Ahi, Alo);
         Split (B, Bhi, Blo);

         E := ((Ahi * Bhi - P) + Ahi * Blo + Alo * Bhi) + Alo * Blo;

         return (P, E);
      end if;
   end Two_Prod;

   -------------
   -- Two_Sqr --
   -------------

   function Two_Sqr (A : Num) return Double_T is
      Q : constant Num := A * A;

      Hi, Lo, E : Num;

   begin
      if Is_Infinity (Q) or else Is_Zero (Q) then
         return (Q, 0.0);

      else
         Split (A, Hi, Lo);

         E := ((Hi * Hi - Q) + 2.0 * Hi * Lo) + Lo * Lo;

         return (Q, E);
      end if;
   end Two_Sqr;

end Product;
