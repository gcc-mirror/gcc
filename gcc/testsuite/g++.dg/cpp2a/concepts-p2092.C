// P2092R0
// { dg-do compile { target concepts } }

template <class T>
concept Bar = requires (T::type t) { ++t; };

template <class T>
concept Foo = requires { requires { T(); }; }; // { dg-error "" }
