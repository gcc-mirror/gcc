// PR c++/78345
// { dg-do compile { target c++11 } }

struct A
{
  const int i;
} a[1] = []{};			// { dg-error "array.*init" }
