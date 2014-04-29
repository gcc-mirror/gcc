/* { dg-do compile } */
/* { dg-options " -O2 " } */

float
f_1 (float a, float b, float c, float d)
{
  if (a > 0.0)
    return c;
  else
    return 2.0;
}

double
f_2 (double a, double b, double c, double d)
{
  if (a > b)
    return c;
  else
    return d;
}

/* { dg-final { scan-assembler-times "\tfcsel" 2 } } */
