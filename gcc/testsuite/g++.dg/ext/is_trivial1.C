// PR c++/108769
// { dg-do compile { target c++20 } }

template <class T>
struct S {
    S() requires false = default;
};
static_assert(!__is_trivial(S<int>));

template <class T>
struct R {
    R() requires true = default;
};
static_assert(__is_trivial(R<int>));
