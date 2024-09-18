// PR c++/109859
// { dg-do compile { target c++20 } }

template<class>
concept C = true;

template <class = C> // { dg-error "invalid use of concept-name .C." }
int f();
