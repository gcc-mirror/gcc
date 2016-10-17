-- { dg-do compile { target *-*-linux* } }
-- { dg-options "-ftrampolines -gnatws" }
-- { dg-skip-if "standard descriptors" { ia64-*-* powerpc64-*-* } }

procedure Trampoline4 is

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

-- { dg-final { scan-assembler "GNU-stack.*x" } }
