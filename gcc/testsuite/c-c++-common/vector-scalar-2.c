/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

typedef int veci __attribute__ ((vector_size (4 * sizeof (int))));

int c;

void f (veci *a)
{
  *a = *a + ++c;
}

/* { dg-final { scan-tree-dump-times " \\\+ 1" 1 "gimple" } } */
