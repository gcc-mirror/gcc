/* { dg-do run } */
/* { dg-options "-O0 -fopenmp-simd" } */

#include <stdio.h>
#define ASSERT_EQ(var, val) if (var != val) { fprintf (stderr, "%s:%d: Unexpected value %d\n", __FILE__, __LINE__, var); \
    __builtin_abort (); }

#define ASSERT_EQ_PTR(var, ptr) if (var != ptr) { fprintf (stderr, "%s:%d: Unexpected value %p\n", __FILE__, __LINE__, var); \
    __builtin_abort (); }

int
main ()
{
  int iter_count;
  int data[10] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

  int iter = 0;
  int *i;
  #pragma omp tile sizes(1)
  for (i = data; i < data + 10; i=i+2)
	{
	  ASSERT_EQ_PTR (i, data + 2 * iter);
	  ASSERT_EQ (*i, data[2 * iter]);
	  iter++;
	}

  unsigned long real_iter_count = ((unsigned long)i - (unsigned long)data) / (sizeof (int) * 2);
  ASSERT_EQ (real_iter_count, 5);

  return 0;
}
