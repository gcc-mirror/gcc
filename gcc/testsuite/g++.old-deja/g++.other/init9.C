// Build don't link:

// Based on a testcase submitted by Tudor Hulubei <tudor@cs.unh.edu>

// X is not a POD because it has a user-defined destructor.
// Therefore, we can't cross its initialization.

// vector<int> is not even an aggregate; nevertheless, no error is
// reported...

struct A {
  A() {}
};

void a() {
  goto bar; // ERROR - jump from here
  A x; // ERROR - jump crosses initialization
 bar: // ERROR - jump to here
  ;
}

struct X {
  ~X() {}
};

void b() {
  goto bar; // ERROR - jump from here
  X x; // ERROR - jump crosses initialization
 bar: // ERROR - jump to here
  ;
}

#include <vector>

void c() {
  goto bar; // ERROR - jump from here
  vector<int> x; // ERROR - jump crosses initialization
 bar: // ERROR - jump to here
  ;
}
