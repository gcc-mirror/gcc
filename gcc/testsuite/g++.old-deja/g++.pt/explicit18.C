// Build don't run:
// GROUPS passed templates
// Special g++ Options: -ansi -pedantic-errors -w
template <class T>
int foo(T t) { return 0; }

int foo(int i);

int main()
{
  return foo<int>(3.0);
}
