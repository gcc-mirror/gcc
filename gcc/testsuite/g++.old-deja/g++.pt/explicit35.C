// Build don't run:
// GROUPS passed templates
// Special g++ Options: -ansi -pedantic-errors -w
struct S 
{
  template <class T>
  void foo(T t);
};

template <>
void S::foo<int>(int i) { }

int main()
{
  S s;
  s.template foo<int>(3.0);
}
