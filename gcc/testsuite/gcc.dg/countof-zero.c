/* { dg-do run } */
/* { dg-options "-std=gnu2y" } */

#define assert(e)  ((e) ? (void) 0 : __builtin_abort ())

void
vla (void)
{
  unsigned n;

  n = 0;
  int z[n];
  assert (_Countof (z) == 0);
}

void
matrix_vla (void)
{
  int i;

  i = 0;
  assert (_Countof (int [i++][4]) == 0);
  assert (i == 0 + 1);
}

int
main (void)
{
  vla ();
  matrix_vla ();
}
