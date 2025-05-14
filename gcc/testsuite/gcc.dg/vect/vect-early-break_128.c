/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" { target { vect_partial_vectors && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump-not "vectorizing stmts using SLP" "vect" { target { { ! vect_partial_vectors } || { ! vect_load_lanes } } } } } */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */

#ifndef N
#define N 800
#endif
unsigned vect_a[N];
unsigned vect_b[N];
  
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i+=2)
 {
   vect_b[i] = x + i;
   vect_b[i+1] = x + i+1;
   if (vect_a[i]*2 != x)
     break;
   if (vect_a[i+1]*2 != x)
     break;
   vect_a[i] = x;
   vect_a[i+1] = x;
   
 }
 return ret;
}
