-- { dg-do compile }

with Discr36_Pkg;

package body Discr36 is

  function N return Natural is begin return 0; end;

  type Arr is array (1 .. N) of R;

  function My_Func is new Discr36_Pkg.Func (Arr);

  procedure Proc is
    A : constant Arr := My_Func;
  begin
    null;
  end;

end Discr36;
