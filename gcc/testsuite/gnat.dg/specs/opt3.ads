-- { dg-do compile }
-- { dg-options "-O3" }

with Ada.Containers.Vectors;
with Opt3_Pkg;

package Opt3 is

  type Arr is array (1 .. Opt3_Pkg.Max) of Integer;

  package Arr_Container is new Ada.Containers.Vectors (Natural, Arr);

end Opt3;
