// Build don't run:
// GROUPS passed templates
template <class T>
struct S
{
  template <class U>
  static double foo(U u) { return (double) u; }
};


int main()
{
  double d = S<int>::template foo<char>(3.3);

  return (d >= 3.1);
}
