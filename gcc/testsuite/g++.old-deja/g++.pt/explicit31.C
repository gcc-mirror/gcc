// { dg-do link  }
// { dg-options "-ansi -pedantic-errors -w" }
// GROUPS passed templates
template <class T>
struct S
{
  template <class U>
  static double foo(U u) { return (double) u; }
};


int main()
{
  double d = S<int>::foo<char>(3.3);

  return (d >= 3.1);
}
