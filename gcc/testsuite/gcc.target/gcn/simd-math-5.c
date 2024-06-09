/* Test that the auto-vectorizer uses the libgcc vectorized division and
   modulus functions.  */

/* The 'scan-assembler' directives are specific to 64-lane vectors.
   { dg-additional-options --param=gcn-preferred-vectorization-factor=64 } */

/* Setting it this way ensures the run tests use the same flag as the
   compile tests.  */
#pragma GCC optimize("O2")

#ifndef TYPE
#define TYPE int
#endif
#ifndef N
#define N 64
#endif

TYPE a[N];
TYPE b[N];

int main()
{
  int i;
  TYPE quot[N], rem[N];
  TYPE quot2[N], rem2[N];
  TYPE refquot[N], refrem[N];

  for (i = 0; i < N; i++)
    {
      a[i] = i * (i >> 2) + (i >> 1);
      b[i] = i;
    }
  __asm__ ("" ::: "memory");

  /* Vector divmod.  */
  for (i = 0; i < N; i++)
    {
      quot[i] = (TYPE)a[i] / (TYPE)b[i];
      rem[i] = (TYPE)a[i] % (TYPE)b[i];
    }
  __asm__ ("" ::: "memory");

  /* Vector div.  */
  for (i = 0; i < N; i++)
    quot2[i] = (TYPE)a[i] / (TYPE)b[i];
  __asm__ ("" ::: "memory");

  /* Vector mod.  */
  for (i = 0; i < N; i++)
    rem2[i] = (TYPE)a[i] % (TYPE)b[i];

  /* Calculate reference values with no vectorization.  */
  for (i = 0; i < N; i++)
    {
      refquot[i] = (TYPE)a[i] / (TYPE)b[i];
      __asm__ ("" ::: "memory");
      refrem[i] = (TYPE)a[i] % (TYPE)b[i];
    }

#ifdef DEBUG
#define DUMP(VAR) \
  __builtin_printf ("%8s: ", #VAR); \
  for (i = 0; i < N; i++) \
    __builtin_printf ("%d ", (int)VAR[i]); \
  __builtin_printf ("\n");
  DUMP (refquot)
  DUMP (quot)
  DUMP (quot2)
  __builtin_printf ("\n");
  DUMP (refrem)
  DUMP (rem)
  DUMP (rem2)
#endif

  for (i = 0; i < N; i++)
    if (quot[i] != refquot[i]
	|| quot2[i] != refquot[i]
	|| rem[i] != refrem[i]
	|| rem2[i] != refrem[i])
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler-times {__divmodv64si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv64si4@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__divv64si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv64si3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__modv64si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv64si3@rel32@lo} 0 } } */
/* { dg-final { scan-assembler-times {__divsi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivsi3@rel32@lo} 0 } } */
