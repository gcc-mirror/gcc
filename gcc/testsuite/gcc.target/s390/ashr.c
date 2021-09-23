/* Test the arithmetic shift right pattern.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

int e(void);

int f (long c, int b)
{
  return (c >> b) && e ();
}
