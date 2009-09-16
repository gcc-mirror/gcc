-- { dg-do compile }

package body Discr20 is

  function Get (X : Wrapper) return Def is
  begin
     return X.It;
  end Get;

end Discr20;
