// PR c++/9228
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

struct A
{
    struct B { B(int); };
};
 
template <typename T> typename A<T>::B foo() { return 1; } // { dg-error "" }
