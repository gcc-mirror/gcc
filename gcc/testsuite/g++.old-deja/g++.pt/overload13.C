// Test that .template limits overload resolution to member templates.

struct A {
  template <class T> int f (T) { return 0; }
  int f (int) { return 1; }
};

int main ()
{
  A a;
  return a.template f (0); // gets bogus error XFAIL *-*-*
}
