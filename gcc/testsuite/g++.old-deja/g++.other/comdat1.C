// Test that statics in inline functions are unified between
// translation units.  Currently we handle this by just suppressing
// inling and relying on unification of the function itself.

// Special g++ Options: -O

// Additional sources: comdat1-aux.cc

inline int f ()
{
  static int i;
  return ++i;
}

int g ();

int main ()
{
  if (f() != 1
      || g() != 2
      || f() != 3)
    return 1;
  return 0;
}
