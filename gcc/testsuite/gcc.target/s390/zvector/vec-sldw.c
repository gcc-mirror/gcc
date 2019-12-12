/* { dg-do run } */
/* { dg-options "-O3 -mzarch -march=z13 -mzvector --save-temps" } */

#include <vecintrin.h>

vector signed char __attribute__((noinline,noclone))
foo8 (vector signed char a)
{
  return vec_sldw (a, (vector signed char){ 0 }, 2);
}

vector signed short __attribute__((noinline,noclone))
foo16 (vector signed short a)
{
  return vec_sldw (a, (vector signed short){ 0 }, 2);
}

vector int __attribute__((noinline,noclone))
foo32 (vector int a)
{
  return vec_sldw (a, (vector int){ 0 }, 2);
}

vector long long __attribute__((noinline,noclone))
foo64 (vector long long a)
{
  return vec_sldw (a, (vector long long){ 0 }, 2);
}

int
main ()
{
  vector long long x;

  x = (vector long long)foo8 ((vector signed char)
	 { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 });
  if (x[0] != 0x08090a0b0c0d0e0fULL || x[1] != 0)
    __builtin_abort ();

  x = (vector long long)foo16 ((vector signed short){ 0, 1, 2, 3, 4, 5, 6, 7 });
  if (x[0] != 0x0004000500060007ULL || x[1] != 0)
    __builtin_abort ();

  x = (vector long long)foo32 ((vector int){ 0, 1, 2, 3 });
  if (x[0] != 0x0000000200000003ULL || x[1] != 0)
    __builtin_abort ();

  x = (vector long long)foo64 ((vector long long){ 0, 1 });
  if (x[0] != 1 || x[1] != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-assembler "vsldb\t%v24,%v24,%v\[0-9\]*,8" } } */
