/* { dg-require-effective-target vect_simd_clones } */
/* { dg-do compile } */

int __attribute__ ((__simd__, const)) fn (int);

void test (int * __restrict__ a, int * __restrict__ b, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int a_;
      if (b[i] > 0)
        a_ = fn (b[i]);
      else
        a_ = b[i] + 5;
      a[i] = a_;
    }
}

/* { dg-final { scan-tree-dump-not {loop contains function calls or data references} "vect" } } */

/* The LTO test produces two dump files and we scan the wrong one.  */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */
