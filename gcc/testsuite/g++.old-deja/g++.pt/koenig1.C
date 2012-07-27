// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Aug 2000 <nathan@codesourcery.com>

// bug 123. We ICEd when koenig lookup found a COMPONENT_REF inside a
// TEMPLATE_ID_EXPR.

void foo(void (*f)());

struct A {
template <int s>
static void g();
template <int s>
void f();         // { dg-message "" } candiate

static void f_plus ()
  {
    foo (f<0>);   // { dg-error "" } no match
    foo (g<0>);
  }
};
