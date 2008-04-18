-- { dg-do compile }
-- { dg-options "-gnatws" }

with Varsize_Return_Pkg1;

package Varsize_Return is

  package P is new Varsize_Return_Pkg1 (Id_T => Natural);

end Varsize_Return;
