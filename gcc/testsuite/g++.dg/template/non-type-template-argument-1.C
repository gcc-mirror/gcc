struct A { static const bool b=false; };

struct B { typedef A X; };

template <bool> struct C {};

template <typename T> struct D
{
   C<T::X> c;                   // { dg-error "names a type" }
};

D<B> d;                         // { dg-error "" }
