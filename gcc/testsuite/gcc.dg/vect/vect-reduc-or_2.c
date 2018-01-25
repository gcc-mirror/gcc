/* Write a reduction loop to be reduced using vector shifts and folded.  */

#include "tree-vect.h"

#if VECTOR_BITS > 128
#define N (VECTOR_BITS / 8)
#else
#define N 16
#endif

extern void abort(void);

int
main (unsigned char argc, char **argv)
{
  unsigned char in[N] __attribute__((aligned(16)));
  unsigned char i = 0;
  unsigned char sum = 1;
  unsigned char expected = 1;

  check_vect ();

  for (i = 0; i < N; i++)
    {
      in[i] = (i + i + 1) & 0xfd;
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N; i++)
    {
      expected |= in[i];
      asm volatile ("" ::: "memory");
    }

  for (i = 0; i < N; i++)
    sum |= in[i];

  if (sum != expected)
    {
      __builtin_printf("Failed %d\n", sum);
      abort();
    }

  return 0;
}

/* { dg-final { scan-tree-dump "Reduce using vector shifts" "vect" { target { whole_vector_shift && { ! vect_logical_reduc } } } } } */
/* { dg-final { scan-tree-dump "Reduce using direct vector reduction" "vect" { target vect_logical_reduc } } } */
