// { dg-do run  }
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Jason Merrill 14 Jun 2001 <jason@redhat.com>

// Test that deduction can add cv-quals to a pointer-to-member type.

struct A;
int A::* pi;

template <typename T> void f (const T A::*) {}

int main ()
{
  f (pi);
}
