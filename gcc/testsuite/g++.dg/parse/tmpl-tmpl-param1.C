// PR c++/7259
// Origin: Philipp Buettgenbach <P.Buettgenbach@FH-Wolfenbuettel.DE>
// Reduced version: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template <template <int> class T> class A : public T<0> {};

template <typename> struct B
{
    template <int> class C {};
    typedef A<C> D;
};

B<void>::D d;
