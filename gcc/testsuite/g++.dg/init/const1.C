// PR c++/20073

template<int> struct A
{
    A();
};

const A<0> x[] = { A<0>() };
