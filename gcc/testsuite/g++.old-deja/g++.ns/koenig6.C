// { dg-do assemble  }
namespace A{
  struct X{};

  X foo(X a){return a;}
  void bar(X*){}
}

int main()
{
  A::X x;
  bar(&foo(x));  // { dg-warning "" } address of temporary
}
