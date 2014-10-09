// PR c++/15339

template<typename> void g3(int, int);
template<typename> void g3(int = 0, int) { }  // { dg-error "may not have default arguments|default argument missing" }
