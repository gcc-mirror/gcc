/* { dg-do run } */
/* { dg-options "-O2 -g" } */

int cnt;

__attribute__((noinline, noclone)) void
bar (void)
{
  cnt++;
}

#include "pr102566-1a.c"

int a;
long int b;
unsigned long int c;
unsigned short int d;

int
main ()
{
  __atomic_store_n (&a, 15, __ATOMIC_RELAXED);
  if (f1 (&a, 2) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 15
      || f1 (&a, 4) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 31)
    __builtin_abort ();
  if (f2 (&a, 1) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 31
      || f2 (&a, 5) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 63)
    __builtin_abort ();
  __atomic_store_n (&b, 24, __ATOMIC_RELAXED);
  if (f3 (&b, 2) != 1 || __atomic_load_n (&b, __ATOMIC_RELAXED) != 28
      || f3 (&b, 3) != 0 || __atomic_load_n (&b, __ATOMIC_RELAXED) != 28)
    __builtin_abort ();
  __atomic_store_n (&a, 0, __ATOMIC_RELAXED);
  if (f4 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 128
      || f4 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 128)
    __builtin_abort ();
  if (f5 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8320
      || f5 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8320)
    __builtin_abort ();
  if (f6 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321
      || f6 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321)
    __builtin_abort ();
  if (cnt != 0
      || (f7 (&a, 7), cnt) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8193
      || (f7 (&a, 7), cnt) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321)
    __builtin_abort ();
  if ((f8 (&a, 7), cnt) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8193
      || (f8 (&a, 7), cnt) != 2 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321)
    __builtin_abort ();
  if (f9 (&a, 13) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 129
      || f9 (&a, 13) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321)
    __builtin_abort ();
  if (f10 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8193
      || f10 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321)
    __builtin_abort ();
  if (f11 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 129
      || f11 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321)
    __builtin_abort ();
  if (f12 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8320
      || f12 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8321)
    __builtin_abort ();
  if (f13 (&a, 7) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8193
      || f13 (&a, 7) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8193)
    __builtin_abort ();
  if (f14 (&a, 13) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 1
      || f14 (&a, 13) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 1)
    __builtin_abort ();
  if (f15 (&a, 0) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 0
      || f15 (&a, 0) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 0)
    __builtin_abort ();
  __atomic_store_n (&a, 8321, __ATOMIC_RELAXED);
  if (f16 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8193
      || f16 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 8193)
    __builtin_abort ();
  if (f17 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 1
      || f17 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 1)
    __builtin_abort ();
  if (f18 (&a) != 1 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 0
      || f18 (&a) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 0)
    __builtin_abort ();
  if (f19 (&c, 7) != 1 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 128
      || f19 (&c, 7) != 0 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 0)
    __builtin_abort ();
  if (f20 (&c) != 0 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 128
      || f20 (&c) != 1 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 0)
    __builtin_abort ();
  __atomic_store_n (&a, 128, __ATOMIC_RELAXED);
  if (f21 (&a, 4) != 0 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 144
      || f21 (&a, 4) != 16 || __atomic_load_n (&a, __ATOMIC_RELAXED) != 144)
    __builtin_abort ();
  __atomic_store_n (&c, 1, __ATOMIC_RELAXED);
  if (f22 (&c) != 128 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 129
      || f22 (&c) != 0 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 1)
    __builtin_abort ();
  if (f23 (&c) != 0 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 129
      || f23 (&c) != 128 || __atomic_load_n (&c, __ATOMIC_RELAXED) != 1)
    __builtin_abort ();
  if (f24 (&d) != 0 || __atomic_load_n (&d, __ATOMIC_RELAXED) != 128
      || f24 (&d) != 1 || __atomic_load_n (&d, __ATOMIC_RELAXED) != 128)
    __builtin_abort ();
  __atomic_store_n (&d, 1, __ATOMIC_RELAXED);
  if (f25 (&d) != 0 || __atomic_load_n (&d, __ATOMIC_RELAXED) != 129
      || f25 (&d) != 1 || __atomic_load_n (&d, __ATOMIC_RELAXED) != 129
      || cnt != 2)
    __builtin_abort ();
  return 0;
}
