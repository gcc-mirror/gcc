// { dg-do assemble  }

// Based on a testcase submitted by Tudor Hulubei <tudor@cs.unh.edu>

// X is not a POD because it has a user-defined destructor.
// Therefore, we can't cross its initialization.

// vector<int> is not even an aggregate; nevertheless, no error is
// reported...

struct A {
  A() {}
};

void a() {
  goto bar; // { dg-message "" } jump from here
  A x; // { dg-message "" } jump crosses initialization
 bar: // { dg-error "" } jump to here
  ;
}

struct X {
  ~X() {}
};

void b() {
  // This was ill-formed until DR 2256.
  goto bar;
  X x;
 bar:
  ;
}

#include <vector>

void c() {
  goto bar; // { dg-message "" } jump from here
  std::vector<int> x; // { dg-message "" } jump crosses initialization
 bar: // { dg-error "" } jump to here
  ;
}
