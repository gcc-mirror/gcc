-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -gnatn -fno-tree-slp-vectorize -fdump-tree-vect-details" }

package body Vect19 is

  function NSum (X : Arr; N : Positive) return Arr is
    Ret : Arr := X;
  begin
    for I in 1 .. N loop
      Ret := Sum (Ret, X);
    end loop;
    return Ret;
  end;

end Vect19;

-- { dg-final { scan-tree-dump "vectorized 1 loops" "vect"  } }
