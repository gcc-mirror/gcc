// Test that we handle mangling of statics in inlines properly.
// { dg-options -fno-weak }
// { dg-do run }

inline int f ()
{
  static int nested;
  nested = 24;
  {
    static int nested;
    nested = 42;
  }
  return (nested != 24);
}

int main()
{
  return f ();
}
