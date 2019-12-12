/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -ftree-vectorize -fdump-tree-vect-details -fno-unroll-loops" } */

#ifndef SIZE
#define SIZE 1024
#endif

#ifndef ALIGN
#define ALIGN 32
#endif

#ifndef TYPE
#define TYPE long long
#endif

#ifndef SIGN_TYPE
#define SIGN_TYPE signed TYPE
#endif

#ifndef UNS_TYPE
#define UNS_TYPE unsigned TYPE
#endif

#define ALIGN_ATTR __attribute__((__aligned__(ALIGN)))

SIGN_TYPE	sa[SIZE] ALIGN_ATTR;
SIGN_TYPE	sb[SIZE] ALIGN_ATTR;
SIGN_TYPE	sc[SIZE] ALIGN_ATTR;

UNS_TYPE	ua[SIZE] ALIGN_ATTR;
UNS_TYPE	ub[SIZE] ALIGN_ATTR;
UNS_TYPE	uc[SIZE] ALIGN_ATTR;

void
sign_lt (SIGN_TYPE val1, SIGN_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] < sc[i]) ? val1 : val2;
}

void
sign_lte (SIGN_TYPE val1, SIGN_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] <= sc[i]) ? val1 : val2;
}

void
sign_gt (SIGN_TYPE val1, SIGN_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] > sc[i]) ? val1 : val2;
}

void
sign_gte (SIGN_TYPE val1, SIGN_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] >= sc[i]) ? val1 : val2;
}


void
uns_lt (UNS_TYPE val1, UNS_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] < uc[i]) ? val1 : val2;
}

void
uns_lte (UNS_TYPE val1, UNS_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] <= uc[i]) ? val1 : val2;
}

void
uns_gt (UNS_TYPE val1, UNS_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] > uc[i]) ? val1 : val2;
}

void
uns_gte (UNS_TYPE val1, UNS_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] >= uc[i]) ? val1 : val2;
}

/* { dg-final { scan-assembler-times {\mvcmpgtsd\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvcmpgtud\M} 4 } } */
/* { dg-final { scan-assembler-not   {\mvcmpequd\M} } } */
/* { dg-final { scan-tree-dump-times "vect_model_simple_cost" 8 "vect" } } */
