/* PR rtl-optimization/70542 */
/* { dg-do run } */

int a[113], d[113];
short b[113], c[113], e[113];

int
main ()
{
  int i;
  long j;
  for (i = 0; i < 113; ++i)
    {
      a[i] = -636544305;
      b[i] = -31804;
    }
  for (j = 1; j <= 112; ++j)
    {
      c[j] = b[j] >> ((a[j] & 1587842570) - 1510214139);
      if (a[j])
	d[j] = j;
      e[j] = 7 << ((2312631697 - b[j]) - 2312663500);
    }
  asm volatile ("" : : : "memory");
  if (c[0] || d[0] || e[0])
    __builtin_abort ();
  for (i = 1; i <= 112; ++i)
    if (c[i] != -1 || d[i] != i || e[i] != 14)
      __builtin_abort ();
  return 0;
}
