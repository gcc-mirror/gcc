// { dg-do assemble  }

template <class T>
class S
{
  template <class U>
  struct S2 {
    S2(const S2<U>& s2u) {}
  };
};
 
