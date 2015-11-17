
/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-ifcvt-details -fno-common -ftree-loop-if-convert-stores" } */

#define LEN 4096
 __attribute__((aligned (32))) float array[LEN];

void test ()
{
  for (int i = 0; i < LEN; i++)
    {
      if (array[i] > (float)0.)
	array[i] = 3;
    }
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
