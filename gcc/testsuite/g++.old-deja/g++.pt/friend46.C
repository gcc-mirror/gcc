// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Nov 2000 <nathan@codesourcery.com>

// bug 43. Two failings, bison parser ickiness caused us to find the member
// named the same as a friend, and then when instantiating, we'd lookup in
// the wrong scope.

namespace X {
  template <class T> class P;
  
  template <class T> void operator- (const P<T>&);
  
  template <class T>
  struct V
  {
    V (const T&);
  
    void operator- ();
    friend void operator-<> (const P<T>& a);
  };
}

int main()
{
  X::V<double> b(1.0);

  return 0;
}
