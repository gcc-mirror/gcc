// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f (int i)
{
  struct S { void g (int j = i) {} }; // ERROR - default argument uses local

  S s;
}

template void f<double>(int);

