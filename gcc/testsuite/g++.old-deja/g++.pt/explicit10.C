// Build don't link:
// GROUPS passed templates
// Special g++ Options: -ansi -pedantic-errors -w
template <class T>
void foo(T t);

int main()
{
  foo<int>(3.0);
}
