// { dg-do compile { target concepts } }

template<class T> requires T::value struct A { };
template<class T> requires T::value struct B { }; // { dg-error "private" }

struct S {
private:
  static constexpr bool value = true;
  template<class T> requires T::value friend struct A;
};

A<S> x;
B<S> y; // { dg-error "constraint" }
