// Build don't link:

// Origin:  Adapted by Nathan Sidwell 29 Apr 1999 <nathan@acm.org>
//          from a test case submitted by Corey Kosak <kosak@cs.cmu.edu>
//          http://egcs.cygnus.com/ml/egcs-bugs/1999-04/msg00502.html

// We should not allow arrays of abstract type. [class.abstract/2]

struct cow_t {
  virtual void f()=0; // ERROR - abstract
};


int main()
{
  cow_t cow[2];  // ERROR - abstract class
  cow[0].f();
  return 0;
}
