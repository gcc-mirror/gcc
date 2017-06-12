// PR c++/18155

template<int> typedef struct A; // { dg-warning "'typedef' was ignored" }
                                // { dg-error "" "" { target *-*-* } .-1 }
