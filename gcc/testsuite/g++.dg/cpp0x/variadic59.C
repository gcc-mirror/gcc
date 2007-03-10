// { dg-options "-std=gnu++0x" }
template<class T, typename... VarArgs>
void print(T t, VarArgs args); // { dg-error "packs not expanded" }
// { dg-error "VarArgs" "" { target *-*-* } 3 }
