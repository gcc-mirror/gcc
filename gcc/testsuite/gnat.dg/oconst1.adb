-- { dg-do compile }
-- { dg-final { scan-assembler-not "elabs" } }

package body OCONST1 is

  procedure check (arg : R) is
  begin
    if arg.u /= 1
       or else arg.b.i1 /= 2
       or else arg.b.i2 /= 3
       or else arg.b.i3 /= 4
    then
      raise Program_Error;
    end if;
  end;

end;

