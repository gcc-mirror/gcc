// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S 
{
  void f (const T&);
  void f (T&);
};

class C 
{
};

typedef int (C::*cp)();

template struct S<cp>;

