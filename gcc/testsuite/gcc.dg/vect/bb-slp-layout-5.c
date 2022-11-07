/* { dg-do compile } */

int a[4], b[4], c[4];

void f1()
{
  a[0] = b[3] - c[3];
  a[1] = b[2] + c[2];
  a[2] = b[1] - c[1];
  a[3] = b[0] + c[0];
}

/* { dg-final { scan-tree-dump "absorbing input layouts" "slp2" { target { vect_int && vect_perm } } } } */
