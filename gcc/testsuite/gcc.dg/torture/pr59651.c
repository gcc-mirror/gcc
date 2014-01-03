/* PR tree-optimization/59561 */
/* { dg-do run } */

extern void abort (void);
int a[] = { 0, 0, 0, 0, 0, 0, 0, 6 };

int b;
int
main ()
{
  for (;;)
    {
      for (b = 7; b; --b)
	a[b] = a[7] > 1;
      break;
    }
  if (a[1] != 0)
    abort ();
  return 0;
}
