// { dg-do assemble  }
// Test that g++ complains about referring to a builtin type in a
// mem-initializer.
// Contributed by Kevin Buhr <buhr@stat.wisc.edu>

int r = 0;

struct foo {		        // { dg-error "" } candidate
  foo(int x) { r = 1; }		// { dg-error "" } candidate
};

struct bar : foo {
  typedef int an_int;
  bar() : bar::an_int(3) {}	// { dg-error "" } not a base
};

int
main() {
  bar b;
  return r;
}
