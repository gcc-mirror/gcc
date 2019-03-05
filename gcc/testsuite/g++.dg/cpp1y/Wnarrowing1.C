// PR c++/78244
// { dg-do compile { target c++14 } }

template<typename>
decltype(int{1.1}) v; // { dg-error "narrowing conversion" }
