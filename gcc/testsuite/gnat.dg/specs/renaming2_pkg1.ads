-- { dg-excess-errors "no code generated" }

with Renaming2_Pkg2;
with Renaming2_Pkg3;
with Renaming2_Pkg4;

package Renaming2_Pkg1 is

  package Impl is new
    Renaming2_Pkg3 (Base_Index_T => Positive, Value_T => Renaming2_Pkg2.Root);

  use Impl;

  package GP is new
    Renaming2_Pkg4 (Length_T => Impl.Length_T, Value_T => Renaming2_Pkg2.Root);

end Renaming2_Pkg1;
