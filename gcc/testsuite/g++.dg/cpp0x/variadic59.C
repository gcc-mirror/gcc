// { dg-options "-std=gnu++11" }
template<class T, typename... VarArgs>
void print(T t, VarArgs args); // { dg-error "packs not expanded" }
// { dg-message "VarArgs" "note" { target *-*-* } 3 }
