// { dg-do assemble  }

template <class T> struct A;

struct B
{
  template <class U>
  friend class A<U>;  // { dg-error "" } does not specialize any args
};

struct C
{
  template <class U>
  friend class A<U*>; // { dg-error "" } partial specialization
};
