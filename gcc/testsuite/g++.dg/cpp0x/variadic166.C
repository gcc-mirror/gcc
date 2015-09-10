// PR c++/67318
// { dg-do compile { target c++11 } }

template<signed...>
struct MyStruct1;

template<unsigned...>
struct MyStruct2;

template<short...>
struct MyStruct3;

template<long...>
struct MyStruct4;
