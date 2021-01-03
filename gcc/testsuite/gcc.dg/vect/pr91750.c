/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int val[1024];
void
foo (int n)
{
  int i;
  for (int j = 0, i = n; j < 1024; ++j, i=(unsigned)i+1)
    val[j] = i;
}

/* Make sure the induction IV uses an unsigned increment.  */
/* { dg-final { scan-tree-dump {vector\([][0-9,]*\) unsigned int} "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
