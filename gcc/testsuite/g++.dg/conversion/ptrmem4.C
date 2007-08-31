// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Assorted pointer to data member c-style cast tests.

struct X {};
struct A { int x; };
struct B : A { int x; };
struct P : A { int x; };
struct V { int x; };
struct D : B, virtual V, private P { int x; };

// Accessible, non-virtual, non-ambiguous base clas.
int B::*p1 = (int B::*)&D::x;
int D::*p2 = (int D::*)&B::x;

// Virtual base class.
int V::*p3 = (int V::*)&D::x;  // { dg-error "" }
int D::*p4 = (int D::*)&V::x;  // { dg-error "" }

// Inaccessible base class.
int P::*p5 = (int P::*)&D::x;
int D::*p6 = (int D::*)&P::x;

// Ambiguous base class.
int A::*p7 = (int A::*)&D::x;  // { dg-error "" }
int D::*p8 = (int D::*)&A::x;  // { dg-error "" }

// Valid conversions which increase cv-qualification.
const int B::*p9 = (const int B::*)&D::x;
const int D::*p10 = (const int D::*)&B::x;

// Valid conversions which decrease cv-qualification.
int B::*p11 = (int B::*)p10;
int D::*p12 = (int D::*)p9;

// Attempts to change member type allowed via reinterpret_cast.
float B::*p13 = (float B::*)&D::x;
float D::*p14 = (float D::*)&B::x;

// Conversion via unrelated classes allwed via reinterpret_cast.
int X::*p15 = (int X::*)&D::x;
