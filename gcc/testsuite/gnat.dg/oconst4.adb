-- { dg-do compile }
-- { dg-final { scan-assembler-not "elabs" } }

package body OCONST4 is

  procedure check (arg : R) is
  begin
    if arg.u /= 1
       or else arg.d.f1 /= 17
       or else arg.d.b.f1 /= one
       or else arg.d.b.f2 /= 2
       or else arg.d.b.f3 /= 17
       or else arg.d.b.f4 /= 42
       or else arg.d.f2 /= one
       or else arg.d.f3 /= 1
       or else arg.d.f4 /= 111
       or else arg.d.i1 /= 2
       or else arg.d.i2 /= 3
    then
      raise Program_Error;
    end if;
  end;

end;
