// { dg-do run  }
// Test for undesired aliasing.

struct A {
  const A * get_this () const { return this; }
};

int main ()
{
  A a;
  int r = 0;
  const A& ar1 = (A)a;
  if (&ar1 == &a)
    r |= 1;
  if (A(a).get_this () == &a)
    r |= 2;
  return r;
}
