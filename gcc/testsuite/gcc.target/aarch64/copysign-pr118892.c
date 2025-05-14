/* { dg-do compile } */
/* { dg-options "-Ofast" } */

double l();
double f()
{
  double t6[2] = {l(), l()};
  double t7[2];
  __builtin_memcpy(&t7, &t6, sizeof(t6));
  return -__builtin_fabs(t7[1]);
}
