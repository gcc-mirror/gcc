/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int
foo (int ko)
{
 int j,i = 0;
  for (j = 0; j < ko; j++)
   i += (i > 10) ? -5 : 7;
 return i;
}

/* { dg-final { scan-tree-dump-times "vectorized 0 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump "Unknown def-use cycle pattern." "vect" } } */
