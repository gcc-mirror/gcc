/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-mcpu=power8 -O2 -ftree-vectorize -fvect-cost-model=dynamic -fno-unroll-loops -fno-unroll-all-loops" } */

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
sign_add (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = sb[i] + sc[i];
}

void
sign_sub (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = sb[i] - sc[i];
}

void
sign_shift_left (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = sb[i] << sc[i];
}

void
sign_shift_right (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = sb[i] >> sc[i];
}

void
sign_max (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] > sc[i]) ? sb[i] : sc[i];
}

void
sign_min (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] < sc[i]) ? sb[i] : sc[i];
}

void
sign_abs (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] < 0) ? -sb[i] : sb[i];	/* xor, vsubudm, vmaxsd.  */
}

void
sign_eq (SIGN_TYPE val1, SIGN_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] == sc[i]) ? val1 : val2;
}

void
sign_lt (SIGN_TYPE val1, SIGN_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    sa[i] = (sb[i] < sc[i]) ? val1 : val2;
}

void
uns_add (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = ub[i] + uc[i];
}

void
uns_sub (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = ub[i] - uc[i];
}

void
uns_shift_left (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = ub[i] << uc[i];
}

void
uns_shift_right (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = ub[i] >> uc[i];
}

void
uns_max (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] > uc[i]) ? ub[i] : uc[i];
}

void
uns_min (void)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] < uc[i]) ? ub[i] : uc[i];
}

void
uns_eq (UNS_TYPE val1, UNS_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] == uc[i]) ? val1 : val2;
}

void
uns_lt (UNS_TYPE val1, UNS_TYPE val2)
{
  unsigned long i;

  for (i = 0; i < SIZE; i++)
    ua[i] = (ub[i] < uc[i]) ? val1 : val2;
}

/* { dg-final { scan-assembler-times "\[\t \]vaddudm\[\t \]"  2 } } */
/* { dg-final { scan-assembler-times "\[\t \]vsubudm\[\t \]"  3 } } */
/* { dg-final { scan-assembler-times "\[\t \]vmaxsd\[\t \]"   2 } } */
/* { dg-final { scan-assembler-times "\[\t \]vmaxud\[\t \]"   1 } } */
/* { dg-final { scan-assembler-times "\[\t \]vminsd\[\t \]"   1 } } */
/* { dg-final { scan-assembler-times "\[\t \]vminud\[\t \]"   1 } } */
/* { dg-final { scan-assembler-times "\[\t \]vsld\[\t \]"     2 } } */
/* { dg-final { scan-assembler-times "\[\t \]vsrad\[\t \]"    1 } } */
/* { dg-final { scan-assembler-times "\[\t \]vsrd\[\t \]"     1 } } */
/* { dg-final { scan-assembler-times "\[\t \]vcmpequd\[\t \]" 2 } } */
/* { dg-final { scan-assembler-times "\[\t \]vcmpgtsd\[\t \]" 1 } } */
/* { dg-final { scan-assembler-times "\[\t \]vcmpgtud\[\t \]" 1 } } */
