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
};


template <class U>
struct C : public B<U>
{
  A_Type Func(); // { dg-error "does not name a type" } implicit typename
};


template <class U>
C<U>::A_Type C<U>::Func() { // { dg-error "typename" } implicit typename
}
