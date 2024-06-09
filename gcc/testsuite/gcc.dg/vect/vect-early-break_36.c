/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

#ifndef N
#define N 803
#endif
unsigned vect_a2[N];
unsigned vect_a1[N];
unsigned vect_b[N];

unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b[i] = x + i;
   if (vect_a1[i]*2 > x)
     break;
   vect_a1[i] = x;
   if (vect_a2[i]*4 > x)
     return i;
   vect_a2[i] = x*x;
 }
 return ret;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */