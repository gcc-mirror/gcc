// Test that partial ordering works between conversion ops from different
// classes.
// From Jason Merrill <jason@cygnus.com>

struct A {
  template <class T> operator T*() { return 0; }
};
struct B : public A {
  template <class T> operator T();
};

int main()
{
  B b;
  (void*) b;
}
