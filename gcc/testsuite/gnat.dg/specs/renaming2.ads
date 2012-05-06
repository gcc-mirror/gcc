-- { dg-do compile }

with Renaming2_Pkg1;

package Renaming2 is

  type T is null record;

  package Iter is new Renaming2_Pkg1.GP.Inner (T);

end Renaming2;
