// Build don't run:
// GROUPS passed membertemplates
struct S
{
  template <class T>
  void foo(T t);
};


template <>
void S::foo<int>(int i)
{
}


int main()
{
  S s;
  s.foo(3);
}
