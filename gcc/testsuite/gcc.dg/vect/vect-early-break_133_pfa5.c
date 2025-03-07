/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

unsigned test4(char x, char *vect, int n)
{  
 unsigned ret = 0;
 for (int i = 0; i < n; i++)
 {
   if (vect[i] > x)
     return 1;

   vect[i] = x;
 }
 return ret;
}

/* { dg-final { scan-tree-dump "Alignment of access forced using peeling" "vect" } } */
