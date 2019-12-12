// DR 2413 - typename in conversion-function-ids.
// { dg-do compile { target c++2a } }

template<class T> struct S {
  operator T::X();
};
