// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f (int i)
{
  struct S { void g (int j = i) {} }; // { dg-error "" } default argument uses local

  S s;
}

template void f<double>(int);

