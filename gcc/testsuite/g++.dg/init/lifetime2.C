// PR c++/26714
// { dg-do run }

extern "C" void abort();

bool ok = false;
struct A
{
  A() { }
  ~A() { if (!ok) abort(); }
};

struct B
{
  const A &a1;
  const A &a2;
  B() : a1(A()),a2(A()) { ok = true; }
};

int main()
{
  B b;
}
