// Build don't link:

template <class T> struct A;

struct B
{
  template <class U>
  friend class A<U>;  // ERROR - does not specialize any args
};

struct C
{
  template <class U>
  friend class A<U*>; // ERROR - partial specialization
};
