/* { dg-require-effective-target whole_vector_shift } */

/* Write a reduction loop to be reduced using vector shifts and folded.  */

#include "tree-vect.h"

extern void abort(void);

int
main (unsigned char argc, char **argv)
{
  unsigned char in[16] __attribute__((aligned(16)));
  unsigned char i = 0;
  unsigned char sum = 1;

  check_vect ();

  for (i = 0; i < 16; i++)
    in[i] = (i + i + 1) & 0xfd;

  for (i = 0; i < 16; i++)
    sum |= in[i];

  if (sum != 29)
    {
      __builtin_printf("Failed %d\n", sum);
      abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump "Reduce using vector shifts" "vect" } } */

