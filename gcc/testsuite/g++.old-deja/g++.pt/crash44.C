// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

// crash test - XFAIL *-*-*

template <class T>
struct S 
{
  template <class U>
  friend S<U>;
};

template struct S<int>;
