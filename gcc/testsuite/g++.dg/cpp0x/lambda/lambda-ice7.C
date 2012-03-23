// PR c++/52487
// { dg-options "-std=c++0x" }

struct A;         // { dg-error "forward declaration" }

void foo(A& a)
{
  [=](){a;};      // { dg-error "invalid use of incomplete type" }
}
