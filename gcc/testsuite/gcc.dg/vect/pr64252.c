/* PR target/64252 */
/* Test correctness of size 3 store groups permutation.  */
/* { dg-additional-options "-O3" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

#include "tree-vect.h"

#define N 50

enum num3
{
  a, b, c
};

struct flags
{
  enum num3 f;
  unsigned int c;
  unsigned int p;
};

struct flagsN
{
  struct flags a[N];
};

void
bar (int n, struct flagsN *ff)
{
  struct flagsN *fc;
  for (fc = ff + 1; fc < (ff + n); fc++)
    {
      int i;
      for (i = 0; i < N; ++i)
       {
         ff->a[i].f = 0;
         ff->a[i].c = i;
         ff->a[i].p = -1;
       }
      for (i = 0; i < n; i++)
       {
         int j;
         for (j = 0; j < N - n; ++j)
           {
             fc->a[i + j].f = 0;
             fc->a[i + j].c = j + i;
             fc->a[i + j].p = -1;
           }
       }
    }
}

struct flagsN q[2];

int main()
{
  int i;
  check_vect ();
  bar(2, q);
  for (i = 0; i < N; i++)
    if (q[0].a[i].f != 0 || q[0].a[i].c != i || q[0].a[i].p != -1)
      abort ();
  return 0;
}
