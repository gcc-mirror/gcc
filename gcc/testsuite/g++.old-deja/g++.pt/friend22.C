// { dg-do assemble  }

template <class T = int>
struct S
{
  template <class U>
  friend class S;
};

template struct S<int>;
