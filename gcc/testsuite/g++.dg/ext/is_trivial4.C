// CWG 1363
// PR c++/85723
// { dg-do compile { target c++11 } }

struct A {
    A() = default;
    A(int i = 0) { }
};

static_assert(!__is_trivial(A), "");
