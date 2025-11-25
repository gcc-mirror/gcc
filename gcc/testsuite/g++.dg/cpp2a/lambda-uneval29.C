// PR c++/116952
// { dg-do compile { target c++20 } }

template<typename, auto> concept A = true;
template<A<[] {}>> int x;
