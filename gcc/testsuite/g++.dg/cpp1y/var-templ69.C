// PR c++/100652
// { dg-do compile { target c++14 } }

template<class...> int var;
template<class... Ts> char var<bool, Ts>; // { dg-error "parameter packs not expanded" }
