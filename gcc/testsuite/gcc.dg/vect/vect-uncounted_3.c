/* Test the correct resetting of live out values on epilog loop entry for IV
   when it's incremented prior to exit taken.  */
/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

extern int N;

int
foo (int c[N], int d[N], int a)
{
  int i = 0;
  while(c[i] != 0)
    {
      i += 1;
      if (d[i] == a)
	break;
      if (d[i] == 7)
	break;
    }
  return i;
}

/* { dg-final { scan-tree-dump "Loop being analyzed as uncounted." "vect" } } */
/* { dg-final { scan-tree-dump "BIT_FIELD_REF <vect_vec_iv_.\[0-9_\]+, \[0-9\]+, 0>" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
