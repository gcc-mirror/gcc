/* { dg-do run } */
/* { dg-options "-fno-common" { target { { hppa*-*-hpux* } && { ! lp64 } } } } */

typedef float V __attribute__ ((vector_size (16)));
V g;
float d[4] = { 4, 3, 2, 1 };

int
main ()
{
  V e;
  __builtin_memcpy (&e, &d, sizeof (d));
  V f = { 5, 15, 25, 35 };
  e = e * f;
  g = e;
  return 0;
}
