/* { dg-do compile } */
/* { dg-options "-O -frounding-math" } */

float
foo (void)
{
  return (float) 0x1699925 * 1.1;
}
