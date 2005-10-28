// { dg-do compile }

// PR c++/22153

template<int> void foo();

template<int> struct A
{
    template<> friend void foo<0>(); // { dg-error "" }
};
