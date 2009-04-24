/* PR rtl-optimization/39794 */
/* { dg-do run } */
/* { dg-options "-O2 -funroll-loops" } */

extern void abort ();

void
foo (int *a, int n)
{
  int i;
  for (i = 0; i < n; i++)
    {
      a[i] *= 2;
      a[i + 1] = a[i - 1] + a[i - 2];
    }
}

int a[16];
int ref[16] = { 0, 1, 4, 2, 10, 12, 24, 44,
		72, 136, 232, 416, 736, 1296, 2304, 2032 };

int
main ()
{
  int i;
  for (i = 0; i < 16; i++)
    a[i] = i;
  foo (a + 2, 16 - 3);
  for (i = 0; i < 16; i++)
    if (ref[i] != a[i])
      abort ();
  return 0;
}
