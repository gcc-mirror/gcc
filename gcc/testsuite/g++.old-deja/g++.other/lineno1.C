// { dg-do assemble  }
// { dg-options "-w" }
// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ was giving the wrong line number for statics.

class A
{
  A();				// { dg-error "" } private
  ~A();				// { dg-error "" } private
};

static A a;			// { dg-error "" } here



