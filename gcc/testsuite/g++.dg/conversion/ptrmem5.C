// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Assorted pointer to member function c-style cast tests.

struct X {};
struct A { int f (); };
struct B : A { int f (); };
struct P : A { int f (); };
struct V { int f (); };
struct D : B, virtual V, private P { int f (); };

// Accessible, non-virtual, non-ambiguous base clas.
int (B::*p1)() = (int (B::*)())&D::f;
int (D::*p2)() = (int (D::*)())&B::f;

// Virtual base class.
int (V::*p3)() = (int (V::*)())&D::f;  // { dg-error "" }
int (D::*p4)() = (int (D::*)())&V::f;  // { dg-error "" }

// Inaccessible base class.
int (P::*p5)() = (int (P::*)())&D::f;
int (D::*p6)() = (int (D::*)())&P::f;

// Ambiguous base class.
int (A::*p7)() = (int (A::*)())&D::f;  // { dg-error "" }
int (D::*p8)() = (int (D::*)())&A::f;  // { dg-error "" }

// Attempts to change member type allowed via reinterpret_cast.
float (B::*p13)() = (float (B::*)())&D::f;
float (D::*p14)() = (float (D::*)())&B::f;

// Conversion via unrelated classes allwed via reinterpret_cast.
int (X::*p15)() = (int (X::*)())&D::f;
