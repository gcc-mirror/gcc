// PR c++/9229
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

template <typename T> class A                               // { dg-message "" }
{
    struct B;
    template <typename U> friend typename A<U,void>::B foo(); // { dg-error "" }
};

template class A<int>;
