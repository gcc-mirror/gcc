-- { dg-do compile }
-- { dg-options "-gnatws -flto" { target lto } }

with Lto3_Pkg1;

package Lto3 is

  package P is new Lto3_Pkg1 (Id_T => Natural);

end Lto3;
