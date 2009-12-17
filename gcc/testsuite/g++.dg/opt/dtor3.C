// PR c++/42386
// { dg-do compile }
// { dg-options "-O2" }
# 1 "A.h" 1
#pragma interface
struct D { virtual bool d () const; };
struct E { virtual ~E (); virtual void *e () const = 0; };
struct A : public D, public E { ~A () {} };
# 5 "dtor3.C" 1
struct F : public A { void *f () const; void *e () const; };
void *F::e () const { return __null; }
