-- { dg-do compile }

package Atomic3 is

   type Four_Bits is mod 2**4;
   type Fourteen_Bits is mod 2**14;
   type Twenty_Eight_Bits is mod 2**28;

   type Rec1 (Mode : Boolean := True) is record
      Reserved : Four_Bits;
      case Mode is
         when True =>
           High_Part : Fourteen_Bits;
           Low_Part  : Fourteen_Bits;
         when False =>
           Data : Twenty_Eight_Bits;
      end case;
   end record;
   for Rec1 use record
      Reserved  at 0 range 28 .. 31;
      High_Part at 0 range 14 .. 27;
      Low_Part  at 0 range  0 .. 13;
      Data      at 0 range  0 .. 27;
   end record;
   for Rec1'Size use 32;
   pragma Unchecked_Union (Rec1);

   type Rec2 is record
      A : Rec1;
      pragma Atomic (A);
   end record;

end Atomic3;
