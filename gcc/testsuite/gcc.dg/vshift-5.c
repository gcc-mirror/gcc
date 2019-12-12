/* { dg-do run } */
/* { dg-options "-O3" } */

extern void abort (void);
long long a[16];

__attribute__((noinline, noclone)) void
f1 (void)
{
  long long a0, a1, a2, a3;
  a0 = a[0];
  a1 = a[1];
  a2 = a[2];
  a3 = a[3];
  a0 = a0 << 2;
  a1 = a1 << 3;
  a2 = a2 << 4;
  a3 = a3 << 5;
  a[0] = a0;
  a[1] = a1;
  a[2] = a2;
  a[3] = a3;
}

__attribute__((noinline, noclone)) void
f2 (void)
{
  long long a0, a1, a2, a3;
  a0 = a[0];
  a1 = a[1];
  a2 = a[2];
  a3 = a[3];
  a0 = a0 << 2;
  a1 = a1 << 2;
  a2 = a2 << 2;
  a3 = a3 << 2;
  a[0] = a0;
  a[1] = a1;
  a[2] = a2;
  a[3] = a3;
}

__attribute__((noinline, noclone)) void
f2a (int x)
{
  long long a0, a1, a2, a3;
  a0 = a[0];
  a1 = a[1];
  a2 = a[2];
  a3 = a[3];
  a0 = a0 << x;
  a1 = a1 << 2;
  a2 = a2 << 2;
  a3 = a3 << 2;
  a[0] = a0;
  a[1] = a1;
  a[2] = a2;
  a[3] = a3;
}

__attribute__((noinline, noclone)) void
f2b (int x)
{
  long long a0, a1, a2, a3;
  a0 = a[0];
  a1 = a[1];
  a2 = a[2];
  a3 = a[3];
  a0 = a0 << 2;
  a1 = a1 << 2;
  a2 = a2 << x;
  a3 = a3 << 2;
  a[0] = a0;
  a[1] = a1;
  a[2] = a2;
  a[3] = a3;
}

__attribute__((noinline, noclone)) void
f3 (int x)
{
  long long a0, a1, a2, a3;
  a0 = a[0];
  a1 = a[1];
  a2 = a[2];
  a3 = a[3];
  a0 = a0 << x;
  a1 = a1 << x;
  a2 = a2 << x;
  a3 = a3 << x;
  a[0] = a0;
  a[1] = a1;
  a[2] = a2;
  a[3] = a3;
}

int
main ()
{
  a[0] = 4LL;
  a[1] = 3LL;
  a[2] = 2LL;
  a[3] = 1LL;
  f1 ();
  if (a[0] != (4LL << 2) || a[1] != (3LL << 3)
      || a[2] != (2LL << 4) || a[3] != (1LL << 5))
    abort ();
  f2 ();
  if (a[0] != (4LL << 4) || a[1] != (3LL << 5)
      || a[2] != (2LL << 6) || a[3] != (1LL << 7))
    abort ();
  f3 (3);
  if (a[0] != (4LL << 7) || a[1] != (3LL << 8)
      || a[2] != (2LL << 9) || a[3] != (1LL << 10))
    abort ();
  f2a (3);
  if (a[0] != (4LL << 10) || a[1] != (3LL << 10)
      || a[2] != (2LL << 11) || a[3] != (1LL << 12))
    abort ();
  f2b (3);
  if (a[0] != (4LL << 12) || a[1] != (3LL << 12)
      || a[2] != (2LL << 14) || a[3] != (1LL << 14))
    abort ();
  return 0;
}
