// PR c++/38
// Origin: Wolfgang Bangerth <bangerth@dealii.org>
// { dg-do compile }

template <int i> struct A
{
    static const int n = 1;
    typedef double X[n];

    A (const X&);
};

template <int i> A<i>::A (const X&) {}
