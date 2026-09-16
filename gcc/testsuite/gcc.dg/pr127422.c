/* PR rtl-optimization/127422 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

static int ordinary_0 = 3;
static int ordinary_1 = 22;
static volatile int effect_0 = 6;
static volatile int effect_1 = 23;
static int ordinary_2 = 41;
__attribute__ ((used)) int *ordinary_escape[]
    = { &ordinary_0, &ordinary_1, &ordinary_2 };

static inline __attribute__ ((always_inline))
unsigned long long
accumulate (unsigned long long acc, unsigned long long val)
{
  return acc + val;
}

unsigned long long
foo (void)
{
  int staged_ordinary_0 = ordinary_0;
  int staged_ordinary_1 = ordinary_1;
  int staged_effect_0 = effect_0;
  int staged_effect_1 = effect_1;
  int staged_ordinary_2 = ordinary_2;
  unsigned long long sum = 0;
  sum = accumulate (sum, staged_ordinary_0);
  sum = accumulate (sum, staged_ordinary_1);
  sum = accumulate (sum, staged_effect_0);
  sum = accumulate (sum, staged_effect_1);
  sum = accumulate (sum, staged_ordinary_2);
  return sum;
}
