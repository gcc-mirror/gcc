// { dg-do run  }
// Test that two extern "C" declarations of the same name in different
// namespaces are treated as declaring the same function.

namespace foo {
  extern "C" int f ();
}

extern "C" int f () { return 0; }

using namespace foo;

int main ()
{
  f ();
}
