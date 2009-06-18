/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

extern int f ();

int
f (double xx, double yy)
{
  if (xx == 42.0 && yy == -42.0)
    return 1;
  return 0;
}
