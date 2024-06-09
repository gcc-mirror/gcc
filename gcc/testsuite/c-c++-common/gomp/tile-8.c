/* { dg-do run } */
/* { dg-options "-O0 -fopenmp-simd" } */

#include <stdio.h>

#define ASSERT_EQ(var, val) \
  do									\
    {									\
      if ((var) != (val))						\
	{								\
	  fprintf (stderr, "%s:%d: Unexpected value %d, expected %d\n",	\
		   __FILE__, __LINE__, (var), (val));			\
	  __builtin_abort ();						\
	}								\
    }									\
  while (0)

int
main ()
{
  int iter_j = 0, iter_k = 0;
  unsigned i, j, k;

  #pragma omp tile sizes(3,5,8)
  for (i = 0; i < 2; i=i+2)
    for (j = 0; j < 3; j=j+1)
      for (k = 0; k < 5; k=k+3)
	{
	  /* fprintf (stderr, "i=%d j=%d k=%d\n", i, j, k);
	     fprintf (stderr, "iter_j=%d iter_k=%d\n", iter_j, iter_k); */
	  ASSERT_EQ (i, 0);
	  if (k == 0)
	    {
	      ASSERT_EQ (j, iter_j);
	      iter_k = 0;
	    }

	  ASSERT_EQ (k, iter_k);

	  iter_k = iter_k + 3;
	  if (k == 3)
	    iter_j++;
	}

  ASSERT_EQ (i, 2);
  ASSERT_EQ (j, 3);
  ASSERT_EQ (k, 6);

  return 0;
}
