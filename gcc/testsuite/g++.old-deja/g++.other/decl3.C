// { dg-do assemble  }

// Origin:  Adapted by Nathan Sidwell 29 Apr 1999 <nathan@acm.org>
//          from a test case submitted by Corey Kosak <kosak@cs.cmu.edu>
//          http://gcc.gnu.org/ml/gcc-bugs/1999-04n/msg00505.html

// We should not allow arrays of abstract type. [class.abstract/2]

struct cow_t {
  virtual void f()=0; // { dg-error "" } abstract
};


int main()
{
  cow_t cow[2];  // { dg-error "" } abstract class
  cow[0].f();
  return 0;
}
