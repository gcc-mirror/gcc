/* { dg-do compile } */
double f(_Complex double a, _Complex double *b, int c)
{
  if (c) return __real__ a;
  return __real__ *b;
}
