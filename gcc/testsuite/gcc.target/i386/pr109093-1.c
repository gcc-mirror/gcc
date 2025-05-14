/* { dg-do run  { target avx2_runtime } }  */
/* { dg-options "-O2 -mavx2 -mtune=znver1 -ftrivial-auto-var-init=zero -fno-stack-protector" } */

int a, b, c, d;
char e, f = 1;
short g, h, i;

__attribute__ ((weak))
void
run (void)
{
  short j;

  for (; g >= 0; --g)
    {
      int *k[10];

      for (d = 0; d < 10; d++)
	k[d] = &b;

      c = *k[1];

      for (; a;)
	j = i - c / f || (e ^= h);
    }
}

int
main (void)
{
  run ();
  return 0;
}
