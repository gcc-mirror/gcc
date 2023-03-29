/* { dg-require-effective-target vect_simd_clones } */
/* { dg-do compile } */

int m;
short int n;

__attribute__ ((simd)) int
foo (void)
{
  m += n;
  m += n;
}

/* { dg-final { scan-tree-dump-not "widen_sum" "vect" } } */
