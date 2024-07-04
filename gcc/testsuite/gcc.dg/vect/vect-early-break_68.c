/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

#ifndef N
#define N 800
#endif
unsigned vect_a1[N];
unsigned vect_b1[N];
unsigned vect_c1[N];
unsigned vect_d1[N];
  
unsigned vect_a2[N];
unsigned vect_b2[N];
unsigned vect_c2[N];
unsigned vect_d2[N];

unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b1[i] += x + i;
   vect_c1[i] += x + i;
   vect_d1[i] += x + i;
   if (vect_a1[i]*2 != x)
     break;
   vect_a1[i] = x;

   vect_b2[i] += x + i;
   vect_c2[i] += x + i;
   vect_d2[i] += x + i;
   if (vect_a2[i]*2 != x)
     break;
   vect_a2[i] = x;

 }
 return ret;
}
