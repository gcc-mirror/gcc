/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1169 6.4.1: Keywords.
   C99 6.4.1(2): Keywords.
   Fixed-point keywords cannot be used in other contexts.  */

int _Fract (void)		/* { dg-error "" } */
{
  return 0;
}

int _Accum (void)		/* { dg-error "" } */
{
  return 0;
}

int _Sat (void)			/* { dg-error "" } */
{
  return 0;
}

int foo1 (int i)
{
  int _Fract = i * 2;		/* { dg-error "" } */
  return _Fract;		/* { dg-error "" } */
}

int foo2 (int i)
{
  int _Accum = i * 2;		/* { dg-error "" } */
  return _Accum;		/* { dg-error "" } */
}

int foo3 (int i)
{
  int _Sat = i * 2;		/* { dg-error "" } */
  return _Sat;			/* { dg-error "" } */
}
