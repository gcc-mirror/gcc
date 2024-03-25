/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

#define N 1024
unsigned vect[N];

unsigned test4(unsigned x)
{
 unsigned ret = 0;
 for (int i = 0; i < N; i++)
 {
   if (i > 16 && vect[i] > x)
     break;

   vect[i] = x;
 }
 return ret;
}
