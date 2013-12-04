-- { dg-do compile }
-- { dg-options "-O" }

package body Opt29 is

  procedure Proc (T : Rec) is
  begin
    if Derived2 (T.F2.all).Id = T.F1.Id then
      raise Program_Error;
    end if;
  end;

end Opt29;
