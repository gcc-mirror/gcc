/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

unsigned test4(char x, char *vect_a, char *vect_b, int n)
{  
 unsigned ret = 0;
 for (int i = 1; i < n; i++)
 {
   if (vect_a[i] > x || vect_b[i] > x)
     return 1;

   vect_a[i] = x;
 }
 return ret;
}

/* { dg-final { scan-tree-dump "Versioning for alignment will be applied" "vect" } } */
