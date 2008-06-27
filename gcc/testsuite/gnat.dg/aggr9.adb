-- { dg-do compile }
-- { dg-options "-O" }

package body Aggr9 is

  procedure Proc (X : R1) is
    M : R2 := (F => X);
  begin
    Send (M);
  end;

end Aggr9;
