/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

unsigned __attribute__((noipa))
f(unsigned x, unsigned y)
{
  {
    __label__ l1;
    void g1(void) { goto l1; }
  l1:;
  }
  _Complex unsigned z = 0, w;
  __imag__ z = x;
  __real__ w = 1;
  __imag__ w = y;
  z = ~(z * w);
  {
    __label__ l2;
    void g2(void) { goto l2; }
    g2();
  l2:;
  }
  return __real__ z;
}

int main ()
{
  if (f(1, 1) != 4294967295)
    __builtin_abort ();
  return 0;
}
