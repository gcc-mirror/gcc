/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

typedef int v4si __attribute__((vector_size(16)));

void foo (v4si *dst, int x)
{
  v4si v[2];
  v[0][0] = 1;
  v[0][1] = x;
  v[0][2] = 2;
  v[0][3] = 3;
  v[0][1] = 0;
  *dst = v[0];
}

/* The shadowed non-constant assign to v[0][1] shouldn't prevent us from
   value-numbering the load to a constant.  */
/* { dg-final { scan-tree-dump "\\*dst_\[0-9\]*\\\(D\\) = { 1, 0, 2, 3 };" "fre1" } } */
