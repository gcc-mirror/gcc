// Test that statics in inline functions are unified between
// translation units.  Currently we handle this by just suppressing
// inling and relying on unification of the function itself.

// Special g++ Options: -O

// Additional sources: comdat2-aux.cc

template <class T>
struct S {
  static int f ()
  {
    static int i;
    return ++i;
  }
  S () {};
  ~S () {};
};

typedef S<int> a;

int g ();

int main ()
{
  if (a::f() != 1
      || g() != 2
      || a::f() != 3)
    return 1;
  return 0;
}
