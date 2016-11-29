/* { dg-do run } */
/* { dg-options "-O3 -save-temps -fno-inline -fno-vect-cost-model" } */

extern void abort ();

void
count_lz_v4si (unsigned *__restrict a, int *__restrict b)
{
  int i;

  for (i = 0; i < 4; i++)
    b[i] = __builtin_clz (a[i]);
}

/* { dg-final { scan-assembler "clz\tv\[0-9\]+\.4s" } } */

int
main ()
{
  unsigned int x[4] = { 0x0, 0xFFFF, 0x1FFFF, 0xFFFFFFFF };
  int r[4] = { 32, 16, 15, 0 };
  int d[4], i;

  count_lz_v4si (x, d);

  for (i = 0; i < 4; i++)
    {
      if (d[i] != r[i])
	abort ();
    }

  return 0;
}

