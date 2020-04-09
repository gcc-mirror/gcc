/* { dg-do run } */
/* { dg-options "-O1" } */

typedef unsigned long V __attribute__ ((__vector_size__ (8)));
typedef _Complex int Ci;
typedef _Complex float Cf;

union U
{
  Ci ci;
  Cf cf;
};

volatile Ci vgi;

Cf foo (Cf c)
{
  __real c = 0x1ffp10;
  return c;
}

Ci ioo (Ci c)
{
  __real c = 50;
  return c;
}


int main (int argc, char *argv[])
{
  union U u;

  __real u.ci = 500;
  __imag u.ci = 1000;
  vgi = u.ci;

  u.ci = ioo (u.ci);
  __imag u.ci = 100;

  if (__real u.ci != 50 || __imag u.ci != 100)
    __builtin_abort();

  u.cf = foo (u.cf);
  __imag u.cf = 0x1p3;

  if (__real u.cf != 0x1ffp10 || __imag u.cf != 0x1p3)
    __builtin_abort();

  return 0;
}
