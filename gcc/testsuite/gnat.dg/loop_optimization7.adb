-- { dg-do compile }
-- { dg-options "-O3" }
-- { dg-options "-O3 -msse" { target i?86-*-* x86_64-*-* } }
-- { dg-require-effective-target sse } 

package body Loop_Optimization7 is

  function Conv (A : Arr) return Arr is
    Result : Arr;
  begin
    for I in A'Range loop
      Result (I) := Conv (A (I));
    end loop;
    return Result;
  end;

end Loop_Optimization7;
