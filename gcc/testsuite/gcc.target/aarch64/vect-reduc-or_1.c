/* { dg-do run } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-all -fno-vect-cost-model" } */
/* Write a reduction loop to be reduced using whole vector right shift.  */

extern void abort (void);

unsigned char in[8] __attribute__((__aligned__(16)));

int
main (unsigned char argc, char **argv)
{
  unsigned char i = 0;
  unsigned char sum = 1;

  for (i = 0; i < 8; i++)
    in[i] = (i + i + 1) & 0xfd;

  /* Prevent constant propagation of the entire loop below.  */
  asm volatile ("" : : : "memory");

  for (i = 0; i < 8; i++)
    sum |= in[i];

  if (sum != 13)
    {
      __builtin_printf ("Failed %d\n", sum);
      abort ();
    }

  return 0;
}

/* { dg-final { scan-tree-dump "Reduce using vector shifts" "vect" } } */
