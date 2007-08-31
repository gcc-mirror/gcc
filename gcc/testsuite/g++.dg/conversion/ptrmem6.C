// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Assorted pointer to data member implicit cast tests.

struct A { int x; };
struct B : A { int x; };
struct P : A { int x; };
struct V { int x; };
struct D : B, virtual V, private P { int x; };

// Valid.
int D::*p1 = &B::x;

// Derived class.
int B::*p2 = &D::x; // { dg-error "" }

// Virtual base class.
int D::*p3 = &V::x; // { dg-error "" }

// Inaccessible base class.
int D::*p4 = &P::x; // { dg-error "" }

// Ambiguous base class.
int D::*p5 = &A::x;  // { dg-error "" }

// Increases cv-qualification.
const int D::*p6 = &B::x;

// Decreases cv-qualification.
int D::*p7 = static_cast<const int D::*>(&D::x); // { dg-error "" }

// Different member type.
float D::*p8 = &B::x;  // { dg-error "" }
