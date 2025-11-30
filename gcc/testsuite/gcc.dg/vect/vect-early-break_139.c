/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#include "tree-vect.h"

__attribute__((noipa))
unsigned loop9(unsigned char *a, unsigned n, unsigned c)
{
  for (unsigned j = 0;;)
    {
      if (c <= j)
        __builtin_abort();

      unsigned char *slot = (unsigned char *)a + j;

      *slot = (char)j;

      unsigned d = j + 1;
      if (d < n)
        j = d;
      else
        return d;
    }
}

int main ()
{
  check_vect ();

  unsigned char buff[16] = {0};
  unsigned res = loop9 (buff, 16, 20);
  if (res != 16)
    __builtin_abort ();
}
