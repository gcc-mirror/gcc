// DR 2446
// { dg-do compile { target c++20 } }

template <typename T> concept C = true;
template <typename T> struct A;
template <> struct A<bool> { using type = bool; };

template <typename T>
void f(A<decltype(C<T>)>::type); // OK, no 'typename' needed
