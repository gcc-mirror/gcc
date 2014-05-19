// PR c++/8785
// Origin: Alexander Zvyagin <Alexander.Zviagine@cern.ch>
// { dg-do compile }

template <int N,typename T> struct A //  { dg-message "" }
{
    typedef T X;
    template <int M> void foo (const A<M,X>&);
};

template <int N,int M,typename T>
void A<N,T>::foo (const A<M,X>&) {} // { dg-error "" }
