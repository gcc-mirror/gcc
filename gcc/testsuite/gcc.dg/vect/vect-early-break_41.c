/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

#ifndef N
#define N 803
#endif
unsigned vect_a[N];
unsigned vect_b[N];
  
unsigned test4(unsigned x)
{
 unsigned ret = 0;
#pragma GCC novector
#pragma GCC unroll 4
 for (int i = 0; i < N; i++)
 {
   vect_b[i] += vect_a[i] + x;
 }
 return ret;
}

/* novector should have blocked vectorization.  */
/* { dg-final { scan-tree-dump-not "vectorized \d loops in function" "vect" } } */