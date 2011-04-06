-- { dg-do compile }
-- { dg-options "-gnatws" }

package body Discr26 is

  function F1 return My_T1 is
    R: My_T1;
  begin
    return R;
  end;

  procedure Proc is
  begin
    if F1.D = 0 then
      raise Program_Error;
    end if;
  end;

end Discr26;
