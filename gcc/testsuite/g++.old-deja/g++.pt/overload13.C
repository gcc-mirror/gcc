// Test that .template limits overload resolution to member templates.

// Note that the standard doesn't seem to require this behavior, but
// EDG works this way.

struct A {
  template <class T> int f (T) { return 0; }
  int f (int) { return 1; }
};

int main ()
{
  A a;
  return a.template f (0); // gets bogus error XFAIL *-*-*
}
