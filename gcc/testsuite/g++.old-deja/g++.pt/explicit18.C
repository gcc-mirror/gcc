// { dg-do link  }
// { dg-options "-ansi -pedantic-errors -w" }
// GROUPS passed templates
template <class T>
int foo(T t) { return 0; }

int foo(int i);

int main()
{
  return foo<int>(3.0);
}
