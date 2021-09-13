// PR c++/82613
// { dg-do compile }

template <typename T> class B;

class A {
  friend class B<A>;
  class Type {};
};

template <typename T>
class B : T::Type { protected: class Type {}; };

B<A> b;

template <typename T>
class C : B<T>::Type, B<T> {};

C<A> c;
