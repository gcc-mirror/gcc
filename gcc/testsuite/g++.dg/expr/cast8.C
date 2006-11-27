// PR c++/29886

struct A {
  static int x[1];
};

void foo(int i)
{
  if (int(A::x[i])) {}
}

