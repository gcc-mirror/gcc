struct A { static const bool b=false; };

struct B { typedef A X; };

template <bool> struct C {};

template <typename T> struct D
{
   C<T::X> c;                   // { dg-error "parsed as a non-type|if a type is meant" }
};

D<B> d;                         // { dg-message "instantiated from here" }
