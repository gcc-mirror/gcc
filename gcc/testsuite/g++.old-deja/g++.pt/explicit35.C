// { dg-do link  }
// { dg-options "-ansi -pedantic-errors -w" }
// GROUPS passed templates
struct S 
{
  template <class T>
  void foo(T t);
};

template <>
void S::foo<int>(int i) { }

int main()
{
  S s;
  s.foo<int>(3.0);
}
