// Build don't run:
// GROUPS passed templates
struct S 
{
  template <class T>
  void foo(T t);

  template <>
  void foo<int>(int i) { }
};

int main()
{
  S s;
  s.template foo<int>(3.0);
}
