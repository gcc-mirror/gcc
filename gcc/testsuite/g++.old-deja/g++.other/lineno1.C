// Submitted by Nathan Sidwell <nathan@acm.org>
// Bug: g++ was giving the wrong line number for statics.
// Special g++ Options: -w

class A
{
  A();				// ERROR - private
  ~A();				// ERROR - private
};

static A a;			// ERROR - here



