/* { dg-do run } */
/* { dg-options "-O2 --save-temps -fno-inline" } */

extern void abort (void);
typedef unsigned int u32;

u32
ngc_si (u32 a, u32 b, u32 c, u32 d)
{
  a = -b - (c < d);
  return a;
}

typedef unsigned long long u64;

u64
ngc_si_tst (u64 a, u32 b, u32 c, u32 d)
{
  a = -b - (c < d);
  return a;
}

u64
ngc_di (u64 a, u64 b, u64 c, u64 d)
{
  a = -b - (c < d);
  return a;
}

int
main ()
{
  int x;
  u64 y;

  x = ngc_si (29, 4, 5, 4);
  if (x != -4)
    abort ();

  x = ngc_si (1024, 2, 20, 13);
  if (x != -2)
    abort ();

  y = ngc_si_tst (0x130000029ll, 32, 50, 12);
  if (y != 0xffffffe0)
    abort ();

  y = ngc_si_tst (0x5000500050005ll, 21, 2, 14);
  if (y != 0xffffffea)
    abort ();

  y = ngc_di (0x130000029ll, 0x320000004ll, 0x505050505ll, 0x123123123ll);
  if (y != 0xfffffffcdffffffc)
    abort ();

  y = ngc_di (0x5000500050005ll,
	      0x2111211121112ll, 0x0000000002020ll, 0x1414575046477ll);
  if (y != 0xfffdeeedeeedeeed)
    abort ();

  return 0;
}

/* { dg-final { scan-assembler-times "ngc\tw\[0-9\]+, w\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "ngc\tx\[0-9\]+, x\[0-9\]+" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
