/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* This will fail because we cannot SLP the load groups yet.  */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { vect_partial_vectors && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" { target { { ! vect_partial_vectors } || { ! vect_load_lanes } } } } } */

#define N 1024
unsigned vect_a[N];
unsigned vect_b[N];
  
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < (N/2); i+=2)
 {
   vect_b[i] = x + i;
   vect_b[i+1] = x + i+1;
   if (vect_a[i] > x || vect_a[i+1] > x)
     break;
   vect_a[i] += x * vect_b[i];
   vect_a[i+1] += x * vect_b[i+1]; 
 }
 return ret;
}
