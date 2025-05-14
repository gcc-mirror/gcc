/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-additional-options "-Ofast" } */

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target { vect_partial_vectors && vect_load_lanes } } } } */
/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" { target { { ! vect_partial_vectors } || { ! vect_load_lanes } } } } } */

char vect_a[1025];
char vect_b[1025];

unsigned test4(char x, int n)
{  
 unsigned ret = 0;
 for (int i = 1; i < (n - 2); i+=2)
 {
   if (vect_a[i] > x || vect_a[i+1] > x)
     return 1;

   vect_b[i] = x;
   vect_b[i+1] = x+1;
 }
 return ret;
}
