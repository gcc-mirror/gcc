// PR target/6087
// The code that moves around insns emitted by reg-stack to cope with
// exception edges lost the REG_DEAD note indicating a pop.  Which
// eventually fills up the register stack resulting in Z == NaN.

// { dg-do run }
// { dg-options "-O" }

extern "C" void abort ();

struct Base
{
  virtual ~Base() {}
};

struct Foo : public Base
{
  Foo ();
};

double x = 3;
double y = 4;

double bar ()
{
  double z = x*x+y*y;
  if (z != 25.0)
    throw 1;
  return z;
}

Foo::Foo ()
{
  bar ();
}

int main ()
{
  try {
    int i;
    for (i = 0; i < 10; ++i)
      new Foo;
  } catch (...) {
    abort ();
  }
  return 0;
}
