/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */

int foo (const char *s, unsigned long n)
{
 unsigned long len = 0;
 while (*s++ && n--)
   ++len;
 return len;
}

