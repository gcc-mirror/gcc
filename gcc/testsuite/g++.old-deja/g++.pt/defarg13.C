// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f (int i)
{
  struct S { void g (int j = i) {} }; // ERROR - default argument uses local

  S s; // ERROR - instantiated here
}

template void f<double>(int); // ERROR - instantiated here

