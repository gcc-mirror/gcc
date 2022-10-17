/* PR tree-optimization/98721 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target alloca } */

int
foo (int n)
{
  if (n <= 0)
    {
      char vla[n];			/* { dg-message "source object 'vla' of size 0" } */
      return __builtin_strlen (vla);	/* { dg-warning "'__builtin_strlen' reading 1 or more bytes from a region of size 0" } */
    }
  return -1;
}
