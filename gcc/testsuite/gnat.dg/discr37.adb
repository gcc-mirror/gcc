-- { dg-do compile }

package body Discr37 is

  procedure Proc (A : access Child) is
    B : Derived renames Derived (A.F(1).all);
    C : Derived renames Derived (B.S(1).all);
  begin
    null;
  end;

end Discr37;
