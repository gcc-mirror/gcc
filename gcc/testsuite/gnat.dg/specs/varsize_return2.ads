-- { dg-do compile }
-- { dg-options "-O" }

with Varsize_Return2_Pkg; use Varsize_Return2_Pkg;

package Varsize_Return2 is

   package My_G is new G (0);

   Result : constant T := My_G.Get;

end Varsize_Return2;
