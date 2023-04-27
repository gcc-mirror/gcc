/* Test that signed division and modulus give the correct result with
   different variations of signedness.  */

/* Setting it this way ensures the run tests use the same flag as the
   compile tests.  */
#pragma GCC optimize("O2")

typedef char v64qi __attribute__ ((vector_size (64)));
typedef short v64hi __attribute__ ((vector_size (128)));
typedef int v64si __attribute__ ((vector_size (256)));
typedef long v64di __attribute__ ((vector_size (512)));

#ifndef TYPE
#define TYPE v64si
#endif
#define N 64

TYPE a;
TYPE b;

int main()
{
  int i;
  TYPE squot, srem;
  TYPE usquot, usrem;
  TYPE vquot, vrem;
  TYPE vquot2, vrem2;
  TYPE refquot, refrem;

  for (i = 0; i < 64; i++)
    {
      a[i] = i * (i >> 2) * (i&1 ? -1 : 1);
      b[i] = i * (i&2 ? -1 : 1);
    }

  for (i = 0; i < N; i++)
    {
      /* Calculate reference values using regular scalar div and mod.  */
      refquot[i] = a[i] / b[i];
      __asm__ ("" ::: "memory");
      refrem[i] = a[i] % b[i];
    }

  __asm__ ("" ::: "memory");
  /* Scalar with divmod.  */
  for (i = 0; i < N; i++)
    {
      squot[i] = a[i] / b[i];
      srem[i] = a[i] % b[i];
    }

  __asm__ ("" ::: "memory");
  /* Vectorized with divmod.  */
  vquot = a / b;
  vrem = a % b;

  __asm__ ("" ::: "memory");
  /* Vectorized with separte div and mod.  */
  vquot2 = a / b;
  __asm__ ("" ::: "memory");
  vrem2 = a % b;

#ifdef DEBUG
#define DUMP(VAR) \
  __builtin_printf ("%8s: ", #VAR); \
  for (i = 0; i < N; i++) \
    __builtin_printf ("%d ", (int)VAR[i]); \
  __builtin_printf ("\n");
  DUMP (refquot)
  DUMP (squot)
  DUMP (vquot)
  DUMP (vquot2)
  __builtin_printf ("\n");
  DUMP (refrem)
  DUMP (srem)
  DUMP (vrem)
  DUMP (vrem2)
  __builtin_printf ("\n");
#endif

  for (i = 0; i < N; i++)
    if (squot[i] != refquot[i]
	|| vquot[i] != refquot[i]
	|| vquot2[i] != refquot[i]
	|| srem[i] != refrem[i]
	|| vrem[i] != refrem[i]
	|| vrem2[i] != refrem[i])
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
