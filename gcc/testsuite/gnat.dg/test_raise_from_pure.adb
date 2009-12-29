-- { dg-do run { xfail arm*-*-* } }
-- { dg-options "-O2" }

-- This is an optimization test and its failure is only a missed optimization.
-- For technical reasons it cannot pass with SJLJ exceptions.

with Raise_From_Pure; use Raise_From_Pure;

procedure test_raise_from_pure is
   K : Integer;
begin
   K := Raise_CE_If_0 (0);
end;
