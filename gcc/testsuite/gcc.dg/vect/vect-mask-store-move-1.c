/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target { i?86-*-* x86_64-*-* } } } */

#define N 256
int p1[N], p2[N], p3[N];
int c[N];
void foo (int n)
{
  int i;
  for (i=0; i<n; i++)
    if (c[i])
      {
	p1[i] += 1;
	p2[i] = p3[i] +2;
      }
}

/* { dg-final { scan-tree-dump-times "Move stmt to created bb" 4 "vect" { target { i?86-*-* x86_64-*-* } xfail { i?86-*-* x86_64-*-* } } } } */
