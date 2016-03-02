
/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-ifcvt-details -ftree-loop-if-convert-stores" } */
/* { dg-require-visibility "" } */

#define LEN 4096
 __attribute__((visibility("hidden"), aligned (32))) float array[LEN] = {};

void test ()
{
  for (int i = 0; i < LEN; i++)
    {
      if (array[i] > (float)0.)
	array[i] = 3;
    }
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
