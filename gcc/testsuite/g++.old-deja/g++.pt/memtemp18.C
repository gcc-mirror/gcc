// Build don't link:
// GROUPS passed templates membertemplates
struct S
{
  template <class T>
  void foo(T) {}
};

template void S::foo(int);

int main()
{
  S s;
  s.foo(3);
}

