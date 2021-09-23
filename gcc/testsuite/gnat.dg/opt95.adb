-- { dg-do run }
-- { dg-options "-O2 -gnatp" }

procedure Opt95 is

  function Foo (J : Integer) return Integer;
  pragma Pure_Function (Foo);
  pragma Machine_Attribute (Foo, "noipa");

  function Foo (J : Integer) return Integer is
  begin
    if J /= 0 then
      raise Constraint_Error;
    end if;
    return 0;
  end;

  function Bar (A : access Integer; N : Integer) return Integer;
  pragma Machine_Attribute (Bar, "noipa");

  function Bar (A : access Integer; N : Integer) return Integer is
    Ret : Integer := 0;
    Ret2 : Integer := 0;
  begin
    if N /= 0 then
      Ret2 := Foo (N);
      Ret := A.all;
    end if;
    Ret := Ret + A.all;
    return Ret + Ret2;
  end;

  V : Integer;
  pragma Volatile (V);

begin
  V := Bar (null, 1);
exception
  when Constraint_Error => null;
end;
