// { dg-do assemble  }
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
class C {
  template <class U>
  friend class ::C;
};

namespace N 
{
template <class T>
class D {
  template <class U>
  friend class N::D;
};
}
