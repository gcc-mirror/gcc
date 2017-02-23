// PR c++/79654
// { dg-do compile { target c++11 } }
// { dg-options "" }

template<typename T> T &make();	// { dg-warning "decomposition declaration only available with" "" { target c++14_down } .+1 }
auto [d1, d2] = make<int>();	// { dg-error "cannot decompose non-array non-class type" }
