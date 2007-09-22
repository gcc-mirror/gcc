/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

/* Cast to union is a GNU C extension.
   Based on the test from ../dfp/.  */

extern void abort (void);

union u
{
  long _Fract lf;
  double d;
};

union n
{
  double d;
  _Fract f;
};

int main ()
{
  static union u u1 = { 0.1lr };
  static union u u2 = { 0.2lr };
  static union u u4 = { 0.0 };

  static union n n1 = { 0.3r };
  static union n n2 = { 3.25 };

  long _Fract lf;
  _Fract f;
  double d;

  if (u1.lf != 0.1lr)
    abort ();

  if (u2.lf != 0.2lr)
    abort ();

  /* cast fixed-point to union type.  */
  lf = 0.4lr;
  f = 0.5r;
  d = 3.25;

  u4 = (union u) lf;
  if (u4.lf != 0.4lr)
    abort ();

  u4 = (union u) d;
  if (u4.d != 3.25)
    abort ();

  n1 = (union n) f;
  if (n1.f != 0.5r)
    abort ();

  n1 = (union n)d;
  if (n1.d != 3.25)
    abort ();

  return 0;
}
