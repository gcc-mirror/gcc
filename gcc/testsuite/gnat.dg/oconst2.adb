-- { dg-do compile }
-- { dg-final { scan-assembler-not "elabs" } }

package body OCONST2 is

  procedure check (arg : R) is
  begin
    if arg.u /= 1
       or else arg.b.i1 /= 2
    then
      raise Program_Error;
    end if;
  end;

end;
