// PR c++/24511 - [DR 470] explicit instantiation/extern template unsats
// on symbols defined later
// { dg-do compile }

template <class T> struct S { T foo (); T bar (); };
template <class T> T S<T>::foo () { return T (); }

#ifndef MAIN

// This part is being tested by the dg-final directive.
template struct S<int>;
#else

// This part is not being tested.
extern template struct S<int>;
int main ()
{
  return S<int>().foo () + S<int>().bar ();
}
#endif

template <class T> T S<T>::bar () { return T (); }

// { dg-final { scan-assembler-not "\\\*UND\\\*\[^\n]*_Z1fIiEvPT_" } }
