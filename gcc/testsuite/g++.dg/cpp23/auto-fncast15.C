// PR c++/112482
// { dg-do compile { target c++23 } }
// { dg-options "-Wno-vexing-parse" }

void foo (auto i, auto j);

struct A {
   A(int,int);
};

void
g (int a)
{
  A b1(auto(42), auto(42));
  A b2(auto(a), auto(42));
  A b3(auto(42), auto(a));
  A b4(auto(a), // { dg-error "13:'auto' parameter" }
       auto(a2)); // { dg-error "13:'auto' parameter" }
  int v1(auto(42));
  int fn1(auto(a)); // { dg-error "16:'auto' parameter" }
}
