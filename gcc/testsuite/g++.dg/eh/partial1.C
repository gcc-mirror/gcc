// PR c++/41449
// { dg-do run }

struct A
{
  A() {}
  A(const A&) { throw 1; }
};

int bs;
struct B
{
  B() { ++bs; }
  B(const B&) { ++bs; }
  ~B() { --bs; }
};

struct C
{
  B b1;
  A a;
  B b2;
};

int main()
{
  {
    B b1, b2;
    A a;

    try {
      C c = { b1, a, b2 };
    } catch (...) {}
  }
  if (bs != 0)
    __builtin_abort ();
}
