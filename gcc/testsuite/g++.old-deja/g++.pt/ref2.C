// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T, T&>
class C;

template <int& I>
class C<int, I> {};

int i;

C<int, i> c;
