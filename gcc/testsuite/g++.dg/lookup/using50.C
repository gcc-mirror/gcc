// PR c++/51319
// { dg-do compile }

template<int> struct X {};

struct Base 
{
    enum { a = 1 };
};

struct Der : Base 
{
    using Base::a;
    typedef X<(int)a> Y;
};
