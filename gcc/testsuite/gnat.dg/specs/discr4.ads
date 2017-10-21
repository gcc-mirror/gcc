-- { dg-do compile }
-- { dg-options "-O" }

with Disc4_Pkg; use Disc4_Pkg;

package Disc4 is

   type Data is record
      Val : Rec;
      Set : Boolean;
   end record;

   type Pair is record
      Lower, Upper : Data;
   end record;

   function Build (L, U : Rec) return Pair is ((L, True), (U, False));

   C1 : constant Pair := Build (Rec_One, Rec_Three);

   C2 : constant Pair := Build (Get (0), Rec_Three);

end Disc4;
