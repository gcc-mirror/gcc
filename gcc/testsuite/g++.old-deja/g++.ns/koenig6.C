// { dg-do assemble  }
namespace A{
  struct X{};

  X* foo(X a);
  void bar(X*){}
}

int main()
{
  A::X x;
  bar(foo(x));
}
