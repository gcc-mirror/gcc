// Build don't run:
// GROUPS passed templates
template <class T>
int foo(T t) { return 1; }

template <>
int foo(int i) { return 0; }

int main()
{
  return (*((int (*)(int)) &foo<int>))(3);
}
