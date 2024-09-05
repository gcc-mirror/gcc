// PR c++/116606
// { dg-do compile }

struct C {
    __attribute__((unavailable)) virtual void f() {}
};

C c;
