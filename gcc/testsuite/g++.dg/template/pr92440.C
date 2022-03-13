// PR c++/92440
// { dg-do compile }

template <int T> // { dg-error "template parameter" }
struct S {
    template <class U> // { dg-message "note: redeclared here as" }
    friend struct S;
};

S<0> s;
