// Origin PR c++/51476
// { dg-do compile { target c++11 } }

template<int> struct A {};                                                               
struct B
{
    int i;
    A<&B::i> a; // { dg-error "could not convert" }
};
