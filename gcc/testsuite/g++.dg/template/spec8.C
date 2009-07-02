// { dg-do compile }

// Specialization of member class template.

template<class T1> struct A
{
  template<class T2> struct B {};
  template<class T2> struct C {};
}; 

template <> template <> struct A<int>::B<int>;
template <> template <class U> struct A<int>::B {};
A<int>::B<int> ab;		// { dg-error "incomplete" }

A<int>::C<char> ac;
template <> template <class U> struct A<int>::C {}; // { dg-error "specialization" }
