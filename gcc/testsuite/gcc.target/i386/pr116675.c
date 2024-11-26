/* { dg-do run } */
/* { dg-options "-O2 -msse2 -mno-ssse3" } */
/* { dg-final { scan-assembler-times "pand" 4 } } */
/* { dg-final { scan-assembler-times "pandn" 4 } } */
/* { dg-final { scan-assembler-times "por" 4 } } */

#include <emmintrin.h>

__attribute__((noinline, noclone, target("sse2")))
static __v8hi foo1 (__v8hi a, __v8hi b)
{
  return __builtin_shufflevector (a, b, 0, 9, 2, 11, 4, 13, 6, 15);
}

__attribute__((noinline, noclone, target("sse2")))
static __v8hi foo2 (__v8hi a, __v8hi b)
{
  return __builtin_shufflevector (a, b, 8, 9, 2, 3, 4, 13, 14, 15);
}

__attribute__((noinline, noclone, target("sse2")))
static __v16qi foo3 (__v16qi a, __v16qi b)
{
  return __builtin_shufflevector (a, b, 0, 17, 2, 19, 4, 21, 6, 23,
			          8, 25, 10, 27, 12, 29, 14, 31);
}

__attribute__((noinline, noclone, target("sse2")))
static __v16qi foo4 (__v16qi a, __v16qi b)
{
  return __builtin_shufflevector (a, b, 0, 1, 2, 3, 4, 21, 6, 23,
                                        8, 25, 10, 27,12,29,14,31);
}

__attribute__((noinline, noclone)) void
compare_v8hi (__v8hi a,  __v8hi b)
{
  for (int i = 0; i < 8; i++) 
    if (a[i] != b[i]) 
      __builtin_abort ();
}

__attribute__((noinline, noclone)) void
compare_v16qi (__v16qi a,  __v16qi b)
{
  for (int i = 0; i < 16; i++)
    if (a[i] != b[i])
      __builtin_abort ();
}

int main (void)
{
  __v8hi s1, s2, s3, s4, s5, s6;
  __v16qi s7, s8, s9, s10, s11, s12;
  s1 = (__v8hi) {0, 1, 2, 3, 4, 5, 6, 7};
  s2 = (__v8hi) {8, 9, 10, 11, 12, 13, 14, 15};
  s7 = (__v16qi) {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  s8 = (__v16qi) {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};

  s3  = foo1 (s1, s2);
  s4  = foo2 (s1, s2);
  s9  = foo3 (s7, s8);
  s10 = foo4 (s7, s8);

  s5 = (__v8hi) {0, 9, 2, 11, 4, 13, 6, 15};
  s6 = (__v8hi) {8, 9, 2, 3, 4, 13, 14, 15};
  s11 = (__v16qi) {0, 17, 2, 19, 4, 21, 6, 23, 8, 25, 10, 27, 12, 29, 14, 31};
  s12 = (__v16qi) {0, 1, 2, 3, 4, 21, 6, 23, 8, 25, 10, 27, 12, 29, 14, 31};

  compare_v8hi (s3, s5);
  compare_v8hi (s4, s6);
  compare_v16qi (s9, s11);
  compare_v16qi (s10, s12);
  return 0;
}
