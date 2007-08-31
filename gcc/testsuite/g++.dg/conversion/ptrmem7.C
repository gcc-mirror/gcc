// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Assorted pointer to member function implicit cast tests.

struct A { int f (); };
struct B : A { int f (); };
struct P : A { int f (); };
struct V { int f (); };
struct D : B, virtual V, private P { int f (); };

// Valid.
int (D::*p1)() = &B::f;

// Derived class.
int (B::*p2)() = &D::f; // { dg-error "" }

// Virtual base class.
int (D::*p3)() = &V::f; // { dg-error "" }

// Inaccessible base class.
int (D::*p4)() = &P::f; // { dg-error "" }

// Ambiguous base class.
int (D::*p5)() = &A::f;  // { dg-error "" }

// Different member type.
float (D::*p6)() = &B::f;  // { dg-error "" }
