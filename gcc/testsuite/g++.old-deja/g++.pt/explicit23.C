// { dg-do link  }
// { dg-options "-ansi -pedantic-errors -w" }
// GROUPS passed templates
template <class T>
int foo(T t) { return 1; }

template <>
int foo<int>(int i) { return 0; }

int main()
{
  return foo<int>(3.0);
}
