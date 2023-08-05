/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fno-tree-loop-distribute-patterns" } */

#include <stdarg.h>
#include "../../tree-vect.h"

#define N 16
struct test {
  char ca[N];
};

extern struct test s;
 
__attribute__((noipa)) int main1 ()
{  
  int i;

  for (i = 0; i < N; i++)
    {
      s.ca[i] = 5;
    }

  /* check results:  */
#pragma GCC novector
  for (i = 0; i < N; i++)
    {
      if (s.ca[i] != 5)
        abort ();
    }

  return 0;
}

int main (void)
{ 
  return main1 ();
} 

/* Peeling to align the store is used. Overhead of peeling is too high.  */
/* { dg-final { scan-tree-dump-times "vectorization not profitable" 1 "vect" { target { ! natural_alignment_32 } } } } */

/* Vectorization occurs, either because overhead of versioning is not
   too high, or because the hardware supports efficient unaligned accesses.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { natural_alignment_32 } } } } */

/* Versioning to align the store is used.  Overhead of versioning is not 
   too high.  */
/* { dg-final { scan-tree-dump-times "loop versioned for vectorization to enhance alignment" 1 "vect" { target { natural_alignment_32 && { ! vect_hw_misalign } } } } } */
