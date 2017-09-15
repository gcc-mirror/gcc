// { dg-options -std=c++17 }

struct A
{
  A();
  A(const A&) = delete;
};

bool b;
A a = A();
A a1 = b ? A() : A();
A a2 = (42, A());

A f();
A a3 = f();
A a4 = b ? A() : f();

void g(A);
A f() {
  g(A());
  if (b)
    throw A();
  else
    return A();
}

A* ap = new A(f());

struct B {
  A a;
  B(): a(A()) {}
};
