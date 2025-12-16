/* { dg-do compile } */
/* { dg-additional-options "-O2" } */

#define ARRAY_LEN 12000

int create (void);

void sort (int *data, int n);

void sort_array (void)
{
  int data[ARRAY_LEN], i;

  for (i=0; i < ARRAY_LEN; ++i)
    {
      data[i] = create ();
    }

  sort (data, ARRAY_LEN);
}

/* { dg-final { scan-assembler-times {\tirg\t} 1 } } */
/* { dg-final { scan-assembler-times {st2g\t...?, \[...?\], 32\n} 2 } } */
