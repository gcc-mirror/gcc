/* PR middle-end/11984 */
/* The following program used to ICE in fold because we didn't check
   whether the constants we were reassociating were integer constants
   before calling tree_int_cst_lt.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double f(double x)
{
  return 1.0 - x - 0.1;
}

