// Build don't run:
// GROUPS passed templates
template <class T>
int foo(T t);

template <>
int foo(int i) { return 0; }

int main()
{
  return foo<int>(3.0);
}
