// { dg-do assemble  }
// Test that g++ complains about referring to a builtin type in a
// mem-initializer.
// Contributed by Kevin Buhr <buhr@stat.wisc.edu>

int r = 0;

struct foo {		        // { dg-message "note" } candidate
  foo(int x) { r = 1; }		// { dg-message "note" }
};

struct bar : foo {
  typedef int an_int;
  bar() : bar::an_int(3) {}	// { dg-error "match" "match" } not a base
  // { dg-message "expected" "exp" { target *-*-* } .-1 }
};

int
main() {
  bar b;
  return r;
}
