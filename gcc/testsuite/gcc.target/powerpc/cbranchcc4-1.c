/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Verify there is no ICE with cbranchcc4 enabled.  */

int foo (double d)
{
  if (d == 0.0)
    return 0;

  d = ((d) >= 0 ? (d) : -(d));

  if (d < 1.0)
    return 1;
}
