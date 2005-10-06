// PR tree-optimization/22488
// This testcase is really a C++ FE bug in represnting virtual inheritance
// It gives the appearance to the middle end that the fields exist twice
// which resulted in a very confused structure analyzer
// { dg-do compile }
// { dg-options "-O" }
struct X
{
    int i0, i1;
      char c;
};

struct A
{
    int i;
      char c0, c1;

        virtual ~A();
};

struct B : virtual A {};

struct C : B
{
    X x;

      void bar(X y) { x = y; }
};

void foo()
{
    C().bar(X());
}
