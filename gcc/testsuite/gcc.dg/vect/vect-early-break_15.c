/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#define N 803
unsigned vect_a[N];
unsigned vect_b[N];
  
int test4(unsigned x)
{
 int ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b[i] = x + i;
   if (vect_a[i] > x)
     return i;
   vect_a[i] += x * vect_b[i];
   
 }
 return ret;
}
