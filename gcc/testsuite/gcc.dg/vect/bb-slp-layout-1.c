/* { dg-do compile } */

int a[4], b[4], c[4], d[4];

void f1()
{
  a[0] = (b[1] << c[3]) - d[1];
  a[1] = (b[0] << c[2]) - d[0];
  a[2] = (b[3] << c[1]) - d[3];
  a[3] = (b[2] << c[0]) - d[2];
}

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 3 "slp2" { target { vect_var_shift && vect_perm } } } } */
