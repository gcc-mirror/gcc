/* { dg-do run } */

struct Foo {
  Foo() {};
  int i;
  short f;
};
struct Bar : public Foo {
  Bar() {};
  short b;
};

extern "C" void abort(void);

int main()
{
  Bar b1, b2;
  b2.i = 0;
  b1.f = 0;
  b1.b = 1;
  b2.f = 1;
  b2.b = 2;
  static_cast<Foo&>(b1) = static_cast<Foo&>(b2);
  if (b1.i != 0 || b1.b != 1)
    abort ();
  if (b1.f != 1)
    abort ();
  return 0;
}
