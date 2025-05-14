// { dg-additional-options "-fmodules-ts -fopenmp-simd" }
// { dg-require-effective-target pthread }

export module foo;
// { dg-module-cmi foo { target pthread } }

export inline void frob (unsigned (&ary)[64])
{
  int sum = 0;

#pragma omp simd safelen(16) aligned (ary : 16)
  for (unsigned ix = 0; ix < 64; ix++)
    ary[ix] *= 2;
}
