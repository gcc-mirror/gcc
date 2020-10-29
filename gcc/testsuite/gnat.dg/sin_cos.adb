--  { dg-do compile }
--  { dg-options "-O2 -gnatn" }

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
package body Sin_Cos is
   procedure Sin_Cos (Angle : T; SinA, CosA : out T) is
   begin
      SinA := Sin (Angle);
      CosA := Cos (Angle);
   end;
end Sin_Cos;

--  { dg-final { scan-assembler "sincos\|cexp" { target *-linux-gnu* *-w64-mingw* *-*-vxworks* } } }
