// Build don't run:
// GROUPS passed templates
// Special g++ Options: -ansi -pedantic-errors -w
template <class T>
int foo(T t);

template <>
int foo<int>(int i) { return 0; }

int main()
{
  return foo<int>(3.0);
}
