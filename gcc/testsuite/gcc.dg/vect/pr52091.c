/* { dg-require-effective-target vect_int } */

/* PR tree-optimization/52091 */

int b, c, d, f;
unsigned h;
extern void abort (void);

int
main ()
{
  d = -1;
  h = 65;
  asm volatile ("" : : : "memory");
  for (f = 0; f < 4; f++)
    {
      h &= (unsigned short) d;
      for (b = 0; b <= 1; b++)
    {
      c = 0;
      d &= 1;
    }
    }
  asm volatile ("" : : : "memory");
  if (b != 2 || c != 0 || d != 1 || f != 4 || h != 1)
    abort ();
  return 0;
}

/* { dg-final { cleanup-tree-dump "vect" } } */

