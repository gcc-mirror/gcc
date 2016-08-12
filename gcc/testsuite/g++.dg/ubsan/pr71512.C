/* PR c/71512 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftrapv -fnon-call-exceptions -fsanitize=undefined" } */

bool
foo (int *x, int *y, int *z)
{
  try
    {
      x[0] = y[0] + z[0];
      x[1] = y[1] - z[1];
      x[2] = y[2] * z[2];
      x[3] = -y[3];
    }
  catch (...)
    {
      return true;
    }
  return false;
}
