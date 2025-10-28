-- { dg-do compile }

with Ada.Containers.Ordered_Sets;
with Generic_Inst5_Pkg1;
with Generic_Inst5_Pkg2;

package Generic_Inst5 is

   package Charsets is new Ada.Containers.Ordered_sets (Character);
   package P1 is new Generic_Inst5_Pkg1 (Charsets);
   package P1_N is new P1.Nested;
   package P2 is new Generic_Inst5_Pkg2 (P1, P1_N);

end Generic_Inst5;
