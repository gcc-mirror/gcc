// Build don't run:
// GROUPS passed templates membertemplates
template <class T>
struct S
{
  template <class U>
  static void f(U u)
  {}
};

int main()
{
  S<int>::f(3);
  S<char>::f("abc");
  S<int>::f("abc");
}
