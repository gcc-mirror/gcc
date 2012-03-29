// PR c++/52746
// { dg-do run }

extern "C" int printf(const char*,...);
extern "C" void abort();
bool db;

struct A
{
  virtual ~A() {}
};

struct B : public A
{
  virtual ~B() { db = true; }
};

template<int> void test()
{
  B * b = new B;
  A * a = b;
  a->~A();
  ::operator delete(b);
}

int main()
{
  test<0>();
  if (!db)
    abort();
}
