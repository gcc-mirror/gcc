// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S 
{
  template <class U>
  friend S<U>;            // ERROR - friend must use tag
};

template struct S<int>;
