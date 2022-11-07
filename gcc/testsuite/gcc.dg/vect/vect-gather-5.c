/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

#ifdef __aarch64__
#pragma GCC target "+sve"
#endif

long a[100], b[100], c[100];

void g1 ()
{
  for (int i = 0; i < 100; i += 2)
    {
      c[i] += a[b[i]] + 1;
      c[i + 1] += a[b[i + 1]] + 2;
    }
}

long g2 ()
{
  long res = 0;
  for (int i = 0; i < 100; i += 2)
    {
      res += a[b[i + 1]];
      res += a[b[i]];
    }
  return res;
}

long g3 ()
{
  long res = 0;
  for (int i = 0; i < 100; i += 2)
    {
      res += a[b[i]];
      res += a[b[i + 1]];
    }
  return res;
}

/* { dg-final { scan-tree-dump-times {add new stmt[^\n]*GATHER_LOAD} 3 "vect" { target aarch64*-*-* } } } */
/* { dg-final { scan-tree-dump-not {add new stmt[^\n]*VEC_PERM_EXPR} "vect" { target aarch64*-*-* } } } */
