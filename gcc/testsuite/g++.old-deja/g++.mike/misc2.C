// { dg-do assemble  }
// { dg-options "-ansi -pedantic" }
// GROUPS passed
/* -ansi -pedantic-errors should catch this. */

class C {
 public:
  extern inline int A() {// { dg-error "" } .*
	return 1;
  }
};
