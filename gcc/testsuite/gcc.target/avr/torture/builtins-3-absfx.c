/* { dg-options "-std=gnu99" } */
/* { dg-do run } */

#include <stdfix.h>

extern void abort (void);

short fract test1_hr (short fract x)
{
  return abshr (x);
}

fract test1_r (fract x)
{
  return absr (x);
}

long fract test1_lr (long fract x)
{
  return abslr (x);
}

long long fract test1_llr (long long fract x)
{
  return absllr (x);
}

short accum test1_hk (short accum x)
{
  return abshk (x);
}

accum test1_k (accum x)
{
  return absk (x);
}

long accum test1_lk (long accum x)
{
  return abslk (x);
}

long long accum test1_llk (long long accum x)
{
  return absllk (x);
}


short fract test2_hr (void)
{
  return abshr (-0.12hr);
}

fract test2_r (void)
{
  return absr (-0.12r);
}

long fract test2_lr (void)
{
  return abslr (-0.12lr);
}

long long fract test2_llr (void)
{
  return absllr (-0.123456llr);
}

short accum test2_hk (void)
{
  return abshk (-221.12hk);
}

accum test2_k (void)
{
  return absk (-4321.12k);
}

long accum test2_lk (void)
{
  return abslk (-4321.12lk);
}

long long accum test2_llk (void)
{
  return absllk (-4321.12llk);
}

#define TEST1(VAL,FX)                                          \
  if (abs ## FX (-VAL ## FX -v) != VAL ## FX + v)              \
    abort();                                                   \
  if (abs ## FX (-VAL ## FX -v) != abs ## FX (VAL ## FX + v))  \
    abort();

#define TEST2(VAL,FX)                                   \
  if (abs ## FX (-VAL ## FX) != VAL ## FX)              \
    abort();                                            \
  if (abs ## FX (-VAL ## FX) != abs ## FX (VAL ## FX))  \
    abort();

#ifndef __FLASH
#define __flash /* empty */
#endif

const __flash short fract volatile v = 0.33hr;
const __flash short fract volatile z = 0hr;

void test1 (void)
{
  TEST1 (0.123, hr);
  TEST1 (0.123, r);
  TEST1 (0.1234567, lr);
  TEST1 (0.1234567, llr);
  
  TEST1 (223.123, hk);
  TEST1 (12345.123, k);
  TEST1 (12342345.123, lk);
  TEST1 (12345.123, llk);
}


void test2 (void)
{
  TEST2 (0.123, hr);
  TEST2 (0.123, r);
  TEST2 (0.1234567, lr);
  TEST2 (0.1234567, llr);
  
  TEST2 (223.123, hk);
  TEST2 (12345.123, k);
  TEST2 (12342345.123, lk);
  TEST2 (12345.123, llk);
}

#define MINMAX(T,FX)                                                    \
  {                                                                     \
    int_ ## FX ## _t imin                                               \
      = (int_ ## FX ## _t) 1 << (8 * sizeof (int_ ## FX ## _t) -1);     \
    int_ ## FX ## _t imax = ~imin;                                      \
    T fmin =  FX ## bits (imin);                                        \
    T fmax =  FX ## bits (imax);                                        \
                                                                        \
    if (abs ## FX (fmin) != fmax)                                       \
      abort();                                                          \
    if (abs ## FX (fmin) != abs ## FX (fmax))                           \
      abort();                                                          \
    if (abs ## FX (fmin + z) != fmax + z)                               \
      abort();                                                          \
    if (abs ## FX (fmin - z) != abs ## FX (fmax + z))                   \
      abort();                                                          \
  }

void test3 (void)
{
  MINMAX (short fract, hr);
  MINMAX (fract, r);
  MINMAX (long fract, lr);
  MINMAX (long long fract, llr);

  MINMAX (short accum, hk);
  MINMAX (accum, k);
  MINMAX (long accum, lk);
  MINMAX (long long accum, llk);
}


int main (void)
{
  test1();
  test2();
  test3();

  return 0;
}

