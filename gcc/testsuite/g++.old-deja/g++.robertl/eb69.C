// Test that g++ complains about referring to a builtin type in a
// mem-initializer.
// Contributed by Kevin Buhr <buhr@stat.wisc.edu>

int r = 0;

struct foo {		        // ERROR - candidate
  foo(int x) { r = 1; }		// ERROR - candidate
};

struct bar : foo {
  typedef int an_int;
  bar() : bar::an_int(3) {}	// ERROR - not a base
};

int
main() {
  bar b;
  return r;
}
