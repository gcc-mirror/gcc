// PR c++/116928
// { dg-do compile { target c++11 } }

template<int = { 0 > 0 }> struct A;
