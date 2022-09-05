/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-vectorize" } */

int a[4], b[4], c[400], d[400];

void f1()
{
  int a0 = a[0] - b[0];
  int a1 = a[1] + b[1];
  int a2 = a[2] - b[2];
  int a3 = a[3] + b[3];
  int b0 = a0;
  int b1 = a1;
  int b2 = a2;
  int b3 = a3;
  for (int i = 0; i < 100; ++i)
    {
      a0 += c[i * 4 + 1];
      a1 += c[i * 4 + 0];
      a2 += c[i * 4 + 3];
      a3 += c[i * 4 + 2];
      b0 ^= d[i * 4 + 3];
      b1 ^= d[i * 4 + 2];
      b2 ^= d[i * 4 + 1];
      b3 ^= d[i * 4 + 0];
    }
  a[0] = a0 ^ b0;
  a[1] = a1 ^ b1;
  a[2] = a2 ^ b2;
  a[3] = a3 ^ b3;
}

/* { dg-final { scan-tree-dump-times "add new stmt: \[^\\n\\r\]* = VEC_PERM_EXPR" 4 "slp1" { target { vect_int && vect_perm } } } } */
/* { dg-final { scan-tree-dump "duplicating permutation node" "slp1" { target { vect_int && vect_perm } } } } */
