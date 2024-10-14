/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */

#define N 1024
unsigned vect_a[N];
unsigned vect_b[N];
 
unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   vect_b[i] = x + i;
   if (vect_a[i] > x)
     {
       ret *= vect_a[i];
       return vect_a[i];
     }
   vect_a[i] = x;
   ret += vect_a[i] + vect_b[i];
 }
 return ret;
}
