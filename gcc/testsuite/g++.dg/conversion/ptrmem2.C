// Copyright (C) 2007 Free Software Foundation
// Contributed by Ollie Wild <aaw@google.com>
// { dg-do compile }

// Assorted pointer to data member static cast tests.

struct A { int x; };
struct B : A { int x; };
struct P : A { int x; };
struct V { int x; };
struct D : B, virtual V, private P { int x; };

// Valid static casts.
int B::*p1 = static_cast<int B::*>(&D::x);
int D::*p2 = static_cast<int D::*>(&B::x);

// Virtual base class.
int V::*p3 = static_cast<int V::*>(&D::x);  // { dg-error "virtual base" }
int D::*p4 = static_cast<int D::*>(&V::x);  // { dg-error "virtual base" }

// Inaccessible base class.
int P::*p5 = static_cast<int P::*>(&D::x);  // { dg-error "inaccessible base" }
// { dg-message "pointer to member function" "" { target *-*-* } .-1 }
int D::*p6 = static_cast<int D::*>(&P::x);  // { dg-error "inaccessible base" }
// { dg-message "pointer to member function" "" { target *-*-* } .-1 }

// Ambiguous base class.
int A::*p7 = static_cast<int A::*>(&D::x);  // { dg-error "ambiguous base" }
// { dg-message "pointer to member function" "" { target *-*-* } .-1 }
int D::*p8 = static_cast<int D::*>(&A::x);  // { dg-error "ambiguous base" }
// { dg-message "pointer to member function" "" { target *-*-* } .-1 }

// Valid conversions which increase cv-qualification.
const int B::*p9 = static_cast<const int B::*>(&D::x);
const int D::*p10 = static_cast<const int D::*>(&B::x);

// Invalid conversions which decrease cv-qualification.
int B::*p11 = static_cast<int B::*>(p10); // { dg-error "15:.static_cast. from type .const int D::\\*. to type .int B::\\*. casts away qualifiers" }
int D::*p12 = static_cast<int D::*>(p9);  // { dg-error "15:.static_cast. from type .const int B::\\*. to type .int D::\\*. casts away qualifiers" }

// Attempts to change member type.
float B::*p13 = static_cast<float B::*>(&D::x); // { dg-error "17:invalid .static_cast." }
float D::*p14 = static_cast<float D::*>(&B::x); // { dg-error "17:invalid .static_cast." }
