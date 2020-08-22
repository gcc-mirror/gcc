/* { dg-do compile } */
/* { dg-options "-O3" } */

/* The pseudo for P is marked as moveable in the IRA pass. */
float
func_0 (float a, float b, float c)
{
  float p = c / a;

  if (b > 1)
    {
      b /= p;
      if (c > 2)
        a /= 3;
    }

  return b / c * a;
}

/* If first_moveable_pseudo and last_moveable_pseudo are not reset correctly,
   they will carry over and spill the pseudo for Q. */
float
func_1 (float a, float b, float c)
{
  float q = a + b;

  c *= a / (b + b);
  if (a > 0)
    c *= q;

  return a * b * c;
}

/* We have plenty of spare registers, so check nothing has been spilled. */
/* { dg-final { scan-assembler-not "\tstr\t" } } */
