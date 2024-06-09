/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

#define N 306
#define NEEDLE 136

int table[N];

__attribute__ ((noipa))
int foo (int i, unsigned short parse_tables_n)
{
  parse_tables_n >>= 9;
  parse_tables_n += 11;
  while (i < N && parse_tables_n--)
    table[i++] = 0;

  return table[NEEDLE];
}

int main ()
{
  check_vect ();

  for (int j = 0; j < N; j++)
    table[j] = -1;

  if (foo (0, 0xFFFF) != 0)
    __builtin_abort ();

  return 0;
}
