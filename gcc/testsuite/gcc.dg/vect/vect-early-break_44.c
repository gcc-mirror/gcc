/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

#ifndef N
#define N 802
#endif
unsigned vect_a[N];
unsigned vect_b[N];
  
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i+=2)
 {
   vect_b[i] = x + i;
   vect_b[i+1] = x + i + 1;
   if (vect_a[i]*2 > x)
     break;
   if (vect_a[i+1]*2 > x)
     break;
   vect_a[i] = x;
   vect_a[i+1] = x;
   
 }
 return ret;
}

/* This will fail because we cannot SLP the load groups yet.  */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" { target { vect_partial_vectors && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump-not "vectorized 1 loops in function" "vect" { target { { ! vect_partial_vectors } || { ! vect_load_lanes } } } } } */
