-- { dg-do compile }
-- { dg-options "-gnat12 -gnato" }

package Cond_Expr1 is

   function Tail (S : String) return String is
     (if S'Last <= S'First then "" else S (S'First + 1 .. S'Last));

end Cond_Expr1;
