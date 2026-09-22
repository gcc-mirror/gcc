/* { dg-do run { target bitint575 } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned _BitInt(256) u256;
typedef signed _BitInt(256) s256;

/* N = 8 has an integer mode everywhere.  */
__attribute__((noipa)) static u256
sext8 (u256 x)
{
  return ((x & 0xff) ^ 0x80) - 0x80;
}

/* N = 128 has a mode only on some targets; N = 191 has none anywhere.
   Both narrow to a signed _BitInt where there is no mode, which the
   bitint lowering still handles.  */
__attribute__((noipa)) static u256
sext128 (u256 x)
{
  u256 m = ((u256) 1) << 127;
  return ((x & (m + m - 1)) ^ m) - m;
}

__attribute__((noipa)) static u256
sext191 (u256 x)
{
  u256 m = ((u256) 1) << 190;
  return ((x & (m + m - 1)) ^ m) - m;
}

__attribute__((noipa)) static u256
sext_ref (u256 x, volatile int *pn)
{
  int n = *pn;
  volatile u256 b = ((u256) 1) << (n - 1);
  volatile u256 v = x & (b + b - 1);
  return (v & b) ? v - (b + b) : v;
}

int
main (void)
{
  volatile int n8 = 8, n128 = 128, n191 = 191;
  u256 v[] = { 0, 1, 0x7f, 0x80, 0xff, ((u256) 1) << 126, ((u256) 1) << 127,
	       ((u256) 1) << 189, ((u256) 1) << 190, ~(u256) 0 };
  unsigned i;

  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    {
      if (sext8 (v[i]) != sext_ref (v[i], &n8))
	__builtin_abort ();
      if (sext128 (v[i]) != sext_ref (v[i], &n128))
	__builtin_abort ();
      if (sext191 (v[i]) != sext_ref (v[i], &n191))
	__builtin_abort ();
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "\\(signed char\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\^ " "optimized" } } */
