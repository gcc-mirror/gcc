/* { dg-do compile } */
/* { dg-options "-O2" } */

void bar (void);

__attribute__((noinline, noclone)) int
f1 (int *a, int bit)
{
  int mask = 1 << bit;
  return (__sync_fetch_and_or (a, mask) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f2 (int *a, int bit)
{
  int mask = 1 << bit;
  int t1 = __atomic_fetch_or (a, mask, __ATOMIC_RELAXED);
  int t2 = t1 & mask;
  return t2 != 0;
}

__attribute__((noinline, noclone)) long int
f3 (long int *a, int bit)
{
  long int mask = 1l << bit;
  return (__atomic_fetch_or (a, mask, __ATOMIC_SEQ_CST) & mask) == 0;
}

__attribute__((noinline, noclone)) int
f4 (int *a)
{
  int mask = 1 << 7;
  return (__sync_fetch_and_or (a, mask) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f5 (int *a)
{
  int mask = 1 << 13;
  return (__atomic_fetch_or (a, mask, __ATOMIC_RELAXED) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f6 (int *a)
{
  int mask = 1 << 0;
  return (__atomic_fetch_or (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

__attribute__((noinline, noclone)) void
f7 (int *a, int bit)
{
  int mask = 1 << bit;
  if ((__sync_fetch_and_xor (a, mask) & mask) != 0)
    bar ();
}

__attribute__((noinline, noclone)) void
f8 (int *a, int bit)
{
  int mask = 1 << bit;
  if ((__atomic_fetch_xor (a, mask, __ATOMIC_RELAXED) & mask) == 0)
    bar ();
}

__attribute__((noinline, noclone)) int
f9 (int *a, int bit)
{
  int mask = 1 << bit;
  return (__atomic_fetch_xor (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f10 (int *a)
{
  int mask = 1 << 7;
  return (__sync_fetch_and_xor (a, mask) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f11 (int *a)
{
  int mask = 1 << 13;
  return (__atomic_fetch_xor (a, mask, __ATOMIC_RELAXED) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f12 (int *a)
{
  int mask = 1 << 0;
  return (__atomic_fetch_xor (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f13 (int *a, int bit)
{
  int mask = 1 << bit;
  return (__sync_fetch_and_and (a, ~mask) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f14 (int *a, int bit)
{
  int mask = 1 << bit;
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_RELAXED) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f15 (int *a, int bit)
{
  int mask = 1 << bit;
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f16 (int *a)
{
  int mask = 1 << 7;
  return (__sync_fetch_and_and (a, ~mask) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f17 (int *a)
{
  int mask = 1 << 13;
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_RELAXED) & mask) != 0;
}

__attribute__((noinline, noclone)) int
f18 (int *a)
{
  int mask = 1 << 0;
  return (__atomic_fetch_and (a, ~mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

__attribute__((noinline, noclone)) long int
f19 (long int *a, int bit)
{
  long int mask = 1l << bit;
  return (__atomic_xor_fetch (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

__attribute__((noinline, noclone)) long int
f20 (long int *a)
{
  long int mask = 1l << 7;
  return (__atomic_xor_fetch (a, mask, __ATOMIC_SEQ_CST) & mask) == 0;
}

__attribute__((noinline, noclone)) int
f21 (int *a, int bit)
{
  int mask = 1 << bit;
  return (__sync_fetch_and_or (a, mask) & mask);
}

__attribute__((noinline, noclone)) long int
f22 (long int *a)
{
  long int mask = 1l << 7;
  return (__atomic_xor_fetch (a, mask, __ATOMIC_SEQ_CST) & mask);
}

__attribute__((noinline, noclone)) long int
f23 (long int *a)
{
  long int mask = 1l << 7;
  return (__atomic_fetch_xor (a, mask, __ATOMIC_SEQ_CST) & mask);
}

__attribute__((noinline, noclone)) short int
f24 (short int *a)
{
  short int mask = 1 << 7;
  return (__sync_fetch_and_or (a, mask) & mask) != 0;
}

__attribute__((noinline, noclone)) short int
f25 (short int *a)
{
  short int mask = 1 << 7;
  return (__atomic_fetch_or (a, mask, __ATOMIC_SEQ_CST) & mask) != 0;
}

/* { dg-final { scan-assembler-times "lock;?\[ \t\]*bts" 9 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btc" 10 } } */
/* { dg-final { scan-assembler-times "lock;?\[ \t\]*btr" 6 } } */
/* { dg-final { scan-assembler-not "cmpxchg" } } */
