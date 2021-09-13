// PR c++/98326
// { dg-do compile { target c++14 } }

struct A {
    A() = default;
    A(const A&) {}
};

void (*fptr)(A) = [](auto){};
