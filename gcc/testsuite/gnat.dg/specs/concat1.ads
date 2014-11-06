-- { dg-do compile }
-- { dg-options "-gnatd.h" }

with Concat1_Pkg; use Concat1_Pkg;

package Concat1 is

   C : constant Natural := Id_For ("_" & Image_Of);

end Concat1;
