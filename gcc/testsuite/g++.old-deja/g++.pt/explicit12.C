// Build don't run:
// GROUPS passed templates
template <class U>
struct S 
{
  template <class T>
  void foo(T t);

  template <>
  void foo<int>(int) {}
};

int main()
{
  S<char*> s;
  s.template foo<int>(3.0);
}
