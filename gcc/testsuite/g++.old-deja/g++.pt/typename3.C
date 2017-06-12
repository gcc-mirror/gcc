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
  A_Type Func(); // { dg-error "does not name a type" "err" } implicit typename
  // { dg-message "note" "note" { target *-*-* } .-1 }
};


template <class U>
B<U>::A_Type B<U>::Func() { // { dg-error "typename" } implicit typename
}
