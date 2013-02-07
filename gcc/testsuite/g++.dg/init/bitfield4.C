// PR c++/56235

struct A
{
  A (const A &);
};

struct B
{
  A a;
  enum Mode { };
  Mode m:8;
};

struct C
{
  C();
  B b;
};

C fn()
{
  return C();
}
