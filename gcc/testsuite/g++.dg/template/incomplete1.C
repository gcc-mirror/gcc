// { dg-do compile }
// Origin: Ivan Godard <igodard at pacbell dot net>
// PR c++/17447: Detect parameters of dependent types even in templates

struct B;   // { dg-error "forward declaration" }
template<typename T> struct A {

    friend A& operator <<(A& a, B b) { return a; } // { dg-error "incomplete" }
    friend A& operator <<(A& a, T b) { return a; }

    void foo1(B b) {}   // { dg-error "incomplete" }
    void foo1a(T b) {}

    B foo2(void) {}  // { dg-error "incomplete" }
    T foo2a(void) {}

    void foo3(B b);
};
