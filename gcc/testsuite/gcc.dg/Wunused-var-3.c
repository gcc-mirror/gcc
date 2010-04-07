/* { dg-do compile } */
/* { dg-options "-Wunused" } */

void
f1 (void)
{
  _Complex int a;	/* { dg-warning "set but not used" } */
  _Complex double b;	/* { dg-warning "set but not used" } */
  __real__ a = 1;
  __imag__ a = 2;
  __real__ b = 3.0;
  __imag__ b = 4.0;
}

int
f2 (void)
{
  _Complex int a;
  _Complex double b;
  __real__ a = 1;
  __imag__ a = 2;
  __real__ b = 3.0;
  __imag__ b = 4.0;
  return __real__ a + __imag__ b;
}

_Complex double
f3 (void)
{
  _Complex int a;
  _Complex double b;
  __real__ a = 1;
  __imag__ a = 2;
  __real__ b = 3.0;
  __imag__ b = 4.0;
  return a + b;
}
