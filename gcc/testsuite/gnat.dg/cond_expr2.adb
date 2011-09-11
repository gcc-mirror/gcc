-- { dg-do compile }
-- { dg-options "-gnat12" }

package body Cond_Expr2 is

  function F (X : integer) return String is
  begin
    return (if X > 0 then "positive" else "negative");
  end;

end Cond_Expr2;
