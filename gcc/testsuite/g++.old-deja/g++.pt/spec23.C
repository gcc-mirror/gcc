/* [temp.expl.spec] p18.  */

template<class T>
struct A {
  template <class U> class B { };
};

template<class T>
class A<T>::B<void> {		// ERROR - only one template header
};
