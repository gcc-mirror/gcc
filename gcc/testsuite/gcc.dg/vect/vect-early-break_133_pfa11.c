/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* Gathers and scatters are not safe to speculate across early breaks.  */
/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

#define N 1024
int vect_a[N];
int vect_b[N];
  
int test4(int x, int stride)
{
 int ret = 0;
 for (int i = 0; i < (N / stride); i++)
 {
   vect_b[i] += x + i;
   if (vect_a[i*stride] == x)
     return i;
   vect_a[i] += x * vect_b[i];
   
 }
 return ret;
}
