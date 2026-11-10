/* Test that signed and unsigned division and modulus use the correct
   vector routines and give the correct results.  */

/* Setting it this way ensures the run tests use the same flag as the
   compile tests.  */
#pragma GCC optimize("O2")

typedef signed char v2qi __attribute__ ((vector_size (2)));
typedef signed char v4qi __attribute__ ((vector_size (4)));
typedef signed char v8qi __attribute__ ((vector_size (8)));
typedef signed char v16qi __attribute__ ((vector_size (16)));
typedef signed char v32qi __attribute__ ((vector_size (32)));
typedef signed char v64qi __attribute__ ((vector_size (64)));

typedef unsigned char v2uqi __attribute__ ((vector_size (2)));
typedef unsigned char v4uqi __attribute__ ((vector_size (4)));
typedef unsigned char v8uqi __attribute__ ((vector_size (8)));
typedef unsigned char v16uqi __attribute__ ((vector_size (16)));
typedef unsigned char v32uqi __attribute__ ((vector_size (32)));
typedef unsigned char v64uqi __attribute__ ((vector_size (64)));

typedef short v2hi __attribute__ ((vector_size (4)));
typedef short v4hi __attribute__ ((vector_size (8)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef short v16hi __attribute__ ((vector_size (32)));
typedef short v32hi __attribute__ ((vector_size (64)));
typedef short v64hi __attribute__ ((vector_size (128)));

typedef unsigned short v2uhi __attribute__ ((vector_size (4)));
typedef unsigned short v4uhi __attribute__ ((vector_size (8)));
typedef unsigned short v8uhi __attribute__ ((vector_size (16)));
typedef unsigned short v16uhi __attribute__ ((vector_size (32)));
typedef unsigned short v32uhi __attribute__ ((vector_size (64)));
typedef unsigned short v64uhi __attribute__ ((vector_size (128)));

typedef int v2si __attribute__ ((vector_size (8)));
typedef int v4si __attribute__ ((vector_size (16)));
typedef int v8si __attribute__ ((vector_size (32)));
typedef int v16si __attribute__ ((vector_size (64)));
typedef int v32si __attribute__ ((vector_size (128)));
typedef int v64si __attribute__ ((vector_size (256)));

typedef unsigned int v2usi __attribute__ ((vector_size (8)));
typedef unsigned int v4usi __attribute__ ((vector_size (16)));
typedef unsigned int v8usi __attribute__ ((vector_size (32)));
typedef unsigned int v16usi __attribute__ ((vector_size (64)));
typedef unsigned int v32usi __attribute__ ((vector_size (128)));
typedef unsigned int v64usi __attribute__ ((vector_size (256)));

typedef long v2di __attribute__ ((vector_size (16)));
typedef long v4di __attribute__ ((vector_size (32)));
typedef long v8di __attribute__ ((vector_size (64)));
typedef long v16di __attribute__ ((vector_size (128)));
typedef long v32di __attribute__ ((vector_size (256)));
typedef long v64di __attribute__ ((vector_size (512)));

typedef unsigned long v2udi __attribute__ ((vector_size (16)));
typedef unsigned long v4udi __attribute__ ((vector_size (32)));
typedef unsigned long v8udi __attribute__ ((vector_size (64)));
typedef unsigned long v16udi __attribute__ ((vector_size (128)));
typedef unsigned long v32udi __attribute__ ((vector_size (256)));
typedef unsigned long v64udi __attribute__ ((vector_size (512)));

#ifndef STYPE
#define STYPE v64si
#define UTYPE v64usi
#endif
#ifndef N
#define N 64
#endif

STYPE a;
STYPE b;
UTYPE ua;
UTYPE ub;

int main()
{
  int i;
  STYPE squot, srem;
  UTYPE usquot, usrem;
  STYPE vquot, vrem;
  UTYPE uvquot, uvrem;
  STYPE vquot2, vrem2;
  UTYPE uvquot2, uvrem2;
  STYPE refquot, refrem;
  UTYPE urefquot, urefrem;

  for (i = 0; i < N; i++)
    {
      a[i] = i * (i >> 2) + (i >> 1);
      ua[i] = a[i];
      b[i] = i;
      ub[i] = i;
    }

  for (i = 0; i < N; i++)
    {
      /* Calculate reference values using regular scalar div and mod.  */
      refquot[i] = a[i] / b[i];
      __asm__ ("" ::: "memory");
      refrem[i] = a[i] % b[i];
      urefquot[i] = ua[i] / ub[i];
      __asm__ ("" ::: "memory");
      urefrem[i] = ua[i] % ub[i];
    }

  __asm__ ("" ::: "memory");
  /* Scalar with divmod.  */
  for (i = 0; i < N; i++)
    {
      squot[i] = a[i] / b[i];
      srem[i] = a[i] % b[i];
      usquot[i] = ua[i] / ub[i];
      usrem[i] = ua[i] % ub[i];
    }

  __asm__ ("" ::: "memory");
  /* Vectorized with divmod.  */
  vquot = a / b;
  vrem = a % b;
  uvquot = ua / ub;
  uvrem = ua % ub;

  __asm__ ("" ::: "memory");
  /* Vectorized with separte div and mod.  */
  vquot2 = a / b;
  __asm__ ("" ::: "memory");
  vrem2 = a % b;
  uvquot2 = ua / ub;
  __asm__ ("" ::: "memory");
  uvrem2 = ua % ub;

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
  DUMP (urefquot)
  DUMP (usquot)
  DUMP (uvquot)
  DUMP (uvquot2)
  __builtin_printf ("\n");
  DUMP (refrem)
  DUMP (srem)
  DUMP (vrem)
  DUMP (vrem2)
  __builtin_printf ("\n");
  DUMP (urefrem)
  DUMP (usrem)
  DUMP (uvrem)
  DUMP (uvrem2)
  __builtin_printf ("\n");
#endif

  for (i = 0; i < N; i++)
    if (squot[i] != refquot[i]
	|| vquot[i] != refquot[i]
	|| vquot2[i] != refquot[i]
	|| usquot[i] != urefquot[i]
	|| uvquot[i] != urefquot[i]
	|| uvquot2[i] != urefquot[i]
	|| srem[i] != refrem[i]
	|| vrem[i] != refrem[i]
	|| vrem2[i] != refrem[i]
	|| usrem[i] != urefrem[i]
	|| uvrem[i] != urefrem[i]
	|| uvrem2[i] != urefrem[i])
      __builtin_abort ();
  return 0;
}

/* { dg-final { scan-assembler-times {__divmodv64si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__udivmodv64si4@rel32@lo} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {__divv64si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivv64si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__modv64si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__umodv64si3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__divsi3@rel32@lo} 1 } } */
/* { dg-final { scan-assembler-times {__udivsi3@rel32@lo} 1 } } */
