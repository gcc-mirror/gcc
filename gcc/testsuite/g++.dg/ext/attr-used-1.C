// Attribute used on a member function or static data member
// of a template should cause them to be instantiated along
// with the class itself.

// { dg-final { scan-assembler "_ZN1AIiE1fEv" } }
// { dg-final { scan-assembler "_ZN1AIiE1tE" } }

template <class T> struct A
{
  void f() __attribute ((used));
  static T t __attribute ((used));
};

template <class T> void A<T>::f() { }
template <class T> T A<T>::t;

A<int> a;
