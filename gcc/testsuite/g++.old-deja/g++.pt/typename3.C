// { dg-do assemble  }
// { dg-options "-Wno-deprecated" }

template <class T>
struct A
{
  typedef T A_Type;
};


template <class U>
struct B : public A<U>
{
  A_Type Func(); // { dg-warning "" } implicit typename
};


template <class U>
B<U>::A_Type B<U>::Func() { // { dg-warning "" } implicit typename
}
