-- { dg-do compile { target *-*-linux* } }
-- { dg-options "-gnatws" }

procedure Trampoline3 is

  A : Integer;

  type FuncPtr is access function (I : Integer) return Integer;

  function F (I : Integer) return Integer is
  begin
    return A + I;
  end F;

  P : FuncPtr := F'Access;
  I : Integer;

begin
  I := P(0);
end;

-- { dg-final { scan-assembler-not "GNU-stack.*x" { xfail hppa*-*-* } } }
