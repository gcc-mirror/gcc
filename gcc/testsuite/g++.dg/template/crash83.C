// PR c++/37650

template<int> struct A {};

template<typename = class A<0>: > struct B {}; // { dg-error "" }
