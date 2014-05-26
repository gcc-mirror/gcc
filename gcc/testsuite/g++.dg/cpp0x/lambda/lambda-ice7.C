// PR c++/52487
// { dg-do compile { target c++11 } }

struct A;         // { dg-message "forward declaration" }

void foo(A& a)
{
  [=](){a;};      // { dg-error "incomplete type" }
}
