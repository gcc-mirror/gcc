-- { dg-do compile }

package body Deferred_Const4 is

  function F return My_Q.T is
    R : My_Q.T;
  begin
    R := My_Q.Null_T;
    return R;
  end;

end Deferred_Const4;
