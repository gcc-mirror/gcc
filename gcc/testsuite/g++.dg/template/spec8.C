// { dg-do compile }

// Specialization of member class template.

template<class T1> struct A
{
  template<class T2> struct B {};
}; 

template <> template <> struct A<int>::B<int> {};
template <> template <class U> struct A<int>::B {}; // { dg-error "specialization" }
