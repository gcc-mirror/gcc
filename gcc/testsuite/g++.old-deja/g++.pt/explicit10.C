// { dg-do assemble  }
// { dg-options "-ansi -pedantic-errors -w" }
// GROUPS passed templates
template <class T>
void foo(T t);

int main()
{
  foo<int>(3.0);
}
