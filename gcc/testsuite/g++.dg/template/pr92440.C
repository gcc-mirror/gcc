// PR c++/92440
// { dg-do compile }

template <int T> // { dg-error "template parameter" }
struct S {
    template <class U>
    friend struct S;  // { dg-message "note: redeclared here as" }
};

S<0> s;
