// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 04 Mar 2002 <nathan@codesourcery.com>
//		  Jason Merrill <jason@redhat.com>

struct A { int i; };
struct B: private A {};
struct C {
  C (int A::*);
};

int A::*aip = &A::i;

void f (int B::*) {}  // should choose this, even though it's ill-formed
void f (C) {}         // even though this would be well-formed

int main ()
{
  f (aip);  // { dg-error "'A' is an inaccessible base of 'B'|conversion" "" }
}
