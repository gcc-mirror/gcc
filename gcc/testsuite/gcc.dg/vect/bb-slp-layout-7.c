/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */

int a[4], b[400];

void f1()
{
  for (int i = 0; i < 100; ++i)
    {
      a[0] += b[i * 4 + 3];
      a[1] += b[i * 4 + 2];
      a[2] += b[i * 4 + 1];
      a[3] += b[i * 4 + 0];
    }
}

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 2 "slp1" { target { vect_int && vect_perm } } } } */
