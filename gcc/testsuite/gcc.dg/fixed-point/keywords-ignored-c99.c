/* { dg-do compile } */
/* { dg-options "-std=c99" } */

/* Fixed-point keywords are not reserved for c99.  */

int _Fract (void)
{
  return 0;
}

int _Accum (void)
{
  return 0;
}

int _Sat (void)
{
  return 0;
}

int foo1 (int i)
{
  int _Fract = i * 2;
  return _Fract;
}

int foo2 (int i)
{
  int _Accum = i * 2;
  return _Accum;
}

int foo3 (int i)
{
  int _Sat = i * 2;
  return _Sat;
}
