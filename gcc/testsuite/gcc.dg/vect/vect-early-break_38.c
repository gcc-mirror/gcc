/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-march=gfx908" { target amdgcn*-*-* } } */

#ifndef N
#define N 803
#endif
unsigned vect_a[N];
unsigned vect_b[N];
  
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i+=2)
 {
   vect_b[i] = x + i;
   if (vect_a[i]*2 > x)
     break;
   vect_a[i] = x;
   
 }
 return ret;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" { target vect_load_lanes } } } */
/* { dg-final { scan-tree-dump-not "vectorized 1 loops in function" "vect" { target { ! vect_load_lanes } } } } */
