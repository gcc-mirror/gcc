// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S 
{
  template <class U>
  friend S<U>;            // { dg-error "" } friend must use tag
};

template struct S<int>;
