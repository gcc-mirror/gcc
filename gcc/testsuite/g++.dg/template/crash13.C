// { dg-do compile }

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/11076: ICE for invalid access declaration containing typename.

template<typename, typename T=void> struct A
{
    typedef A<T,T> B;
};

template <typename T> struct C
{				// { dg-error "not a base type" }
    typedef typename A<T>::B X;
    X::Y;
};

C<void> c;			// { dg-error "instantiated" }
