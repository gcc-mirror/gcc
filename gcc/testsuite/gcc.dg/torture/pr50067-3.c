/* { dg-do run } */
/* { dg-options "-fpredictive-commoning" } */

extern void abort (void);
int a[6] = { 0, 0, 0, 0, 7, 0 };
static int *p = &a[4];

int
main ()
{
  int i;
  for (i = 0; i < 4; ++i)
    {
      a[i + 1] = a[i + 2] > i;
      *p &= ~1;
    }
  if (a[4] != 0)
    abort ();
  return 0;
}
