// Basic runtime test for implicit move constructor
// { dg-do run }
// { dg-options -std=c++0x }

int m;

struct A
{
  A() = default;
  A(A&&) { ++m; }
  A& operator=(A&&) { ++m; return *this; }
};

struct B
{
  B() = default;
  B(const B&);
  B(B&&) { ++m; }
  B& operator=(const B&);
  B& operator=(B&&) { ++m; return *this; }
};

struct C
{
  C() = default;
  C(C&);
  C(C&&) { ++m; }
  C& operator=(C&);
  C& operator=(C&&) { ++m; return *this; }
};

struct D: public A, public B
{
  C c;
  int i;
};

struct E: public A, public B
{
  C c;
  int i;
  E() = default;
  E(E&&) = default;
  E& operator=(E&&) = default;
};

int main()
{
  D d1;
  D d2 (static_cast<D&&>(d1));
  d1 = static_cast<D&&>(d2);
  E e1;
  E e2 (static_cast<E&&>(e1));
  e1 = static_cast<E&&>(e2);
  return m != 12;
}
