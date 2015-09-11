// { dg-do assemble  }
// { dg-options "-w" }
// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ was giving the wrong line number for statics.

class A
{
  A();				// { dg-message "" } private
  ~A();				// { dg-message "" } private
};

static A a;			// { dg-error "" } here



