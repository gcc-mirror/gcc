// { dg-do assemble  }
// Make sure that forward declarations of specializations work...

template <class T> class A { };
template <> class A<int>;
A<int> a;			// { dg-error "" } incomplete type
template <> class A<int> { };
