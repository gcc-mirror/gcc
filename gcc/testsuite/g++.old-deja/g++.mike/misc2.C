// Build don't link: 
// Special g++ Options: -ansi -pedantic
// GROUPS passed
/* -ansi -pedantic-errors should catch this. */

class C {
 public:
  extern inline int A() {// ERROR - .*
	return 1;
  }
};
