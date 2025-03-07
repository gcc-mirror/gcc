/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* This should be vectorizable through load_lanes and linear targets.  */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target vect_load_lanes } } } */

unsigned test4(char x, char * restrict vect_a, char * restrict vect_b, int n)
{  
 unsigned ret = 0;
 for (int i = 0; i < n; i+=2)
 {
   if (vect_a[i] > x || vect_a[i+1] > x)
     return 1;

   vect_b[i] = x;
   vect_b[i+1] = x+1;
 }
 return ret;
}
