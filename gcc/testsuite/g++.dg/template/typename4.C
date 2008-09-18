struct B { template <typename U> struct C; };
template <typename T> struct A { typedef typename T::C V; }; // { dg-error "not a type" }
void f () { A<B>::V p; } // { dg-message "instantiated" }
