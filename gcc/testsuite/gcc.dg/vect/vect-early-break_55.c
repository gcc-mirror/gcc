/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

#ifndef N
#define N 803
#endif
unsigned vect_a[N];
unsigned vect_b[N];
  
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   volatile unsigned tmp = x + i;
   vect_b[i] = tmp;
   if (vect_a[i] > x)
     break;
   vect_a[i] = x;
   
 }
 return ret;
}
