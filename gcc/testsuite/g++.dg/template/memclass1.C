// { dg-do compile }

// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>

// PR c++/10555: ICE for member class template when one of the
// template argument levels contains errors.

template <typename> struct A
{
    template <typename> struct B;
};

template <typename T> struct C
{
    typedef typename A<T>::template B<U> X; // { dg-error "mismatch|expected" }
};

C<void> c;			// { dg-error "instantiated" }
