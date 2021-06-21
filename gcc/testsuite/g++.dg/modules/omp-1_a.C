// { dg-additional-options "-fmodules-ts -fopenmp" }
// { dg-require-effective-target pthread }

export module foo;
// { dg-module-cmi foo { target pthread } }

export inline void frob (unsigned (&ary)[64])
{
  int sum = 0;

#pragma omp for
  for (unsigned ix = 0; ix < 64; ix++)
    sum += ary[ix];

#pragma omp simd safelen(16) aligned (ary : 16)
  for (unsigned ix = 0; ix < 64; ix++)
    ary[ix] *= 2;
}
