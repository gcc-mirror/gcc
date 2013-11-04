// Runtime test for C++11 unrestricted unions
// { dg-options -std=c++11 }
// { dg-do run }

int c, d;
struct A
{
  int i;
  A(): i(1) { ++c; }
  A(const A&): i(2) { ++c; }
  ~A() { ++d; }
};

union B
{
  A a;
  B() { }
  B(const B& b) { }
  ~B() { }
};

struct C
{
  union { A a; };
  C() { }
  C(const C&) { }
  ~C() { }
};

union D
{
  A a;
  D(): a() { }
  D(const D& d): a(d.a) { }
  ~D() { a.~A(); }
};

struct E
{
  union { A a; };
  E(): a() { }
  E(const E& e): a (e.a) { }
  ~E() { a.~A(); }
};

int main()
{
  {
    B b1;
    B b2(b1);

    C c1;
    C c2(c1);
  }

  if (c != 0 || d != 0)
    return c+d*10;

  {
    D d1;
    D d2(d1);

    E e1;
    E e2(e1);
  }

  if (c != 4 || d != 4)
    return c*100+d*1000;
}
