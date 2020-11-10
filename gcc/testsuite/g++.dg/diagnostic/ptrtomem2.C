// PR c++/85901
// { dg-do compile { target c++11 } }

template<class> struct A;

template<class U>
struct A<int U::*> {
    template<class TT>
    static auto c(int U::*p, TT o) -> decltype(o.*p); // { dg-message {A<int U::\*>} }
};

struct X {};

int x = A<int X::*>::c(); // { dg-error "no matching function for call" }
