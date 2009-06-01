-- { dg-do compile }
-- { dg-final { scan-assembler-not "elabs" } }

package body OCONST3 is

  procedure check (arg : R) is
  begin
    if arg.u /= 1
       or else arg.f /= one
       or else arg.b.i1 /= 3
    then
      raise Program_Error;
    end if;
  end;

end;
