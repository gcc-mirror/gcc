// PR c++/8895
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

template <typename X, typename Y = B<X> > struct A // { dg-error "" }
{
    A();
    A(const A&);
}; // { dg-error "" }
