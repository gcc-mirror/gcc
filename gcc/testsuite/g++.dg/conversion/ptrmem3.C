// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Assorted pointer to member function static cast tests.

struct A { int f (); };
struct B : A { int f (); };
struct P : A { int f (); };
struct V { int f (); };
struct D : B, virtual V, private P { int f (); };

// Valid static casts.
int (B::*p1)() = static_cast<int (B::*)()>(&D::f);
int (D::*p2)() = static_cast<int (D::*)()>(&B::f);

// Virtual base class.
int (V::*p3)() = static_cast<int (V::*)()>(&D::f);  // { dg-error "" }
int (D::*p4)() = static_cast<int (D::*)()>(&V::f);  // { dg-error "" }

// Inaccessible base class.
int (P::*p5)() = static_cast<int (P::*)()>(&D::f);  // { dg-error "" }
int (D::*p6)() = static_cast<int (D::*)()>(&P::f);  // { dg-error "" }

// Ambiguous base class.
int (A::*p7)() = static_cast<int (A::*)()>(&D::f);  // { dg-error "" }
int (D::*p8)() = static_cast<int (D::*)()>(&A::f);  // { dg-error "" }

// Attempts to change member type.
float (B::*p13)() = static_cast<float (B::*)()>(&D::f); // { dg-error "21:invalid .static_cast." }
float (D::*p14)() = static_cast<float (D::*)()>(&B::f); // { dg-error "21:invalid .static_cast." }
