// PR c++/112410
// { dg-do compile { target c++23 } }

struct A {
   A(int,int);
};

int a;
A b1(auto(a), 42);
