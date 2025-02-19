// PR c++/118319 - With -fpermissive
// { dg-do "compile" { target c++11 } }
// { dg-additional-options "-fpermissive" }

// Template case, that used to crash.
// Check that we error-out even with -fpermissive.

template <int>
struct S { // { dg-error "instantiating erroneous template" }
  friend void foo1 (int a = []{}());  // { dg-warning "specifies default|only declaration" }
};

void foo1 (int a) {}

void hello (){
  S<0> t;
  foo1 ();
}


// Non template case, that already worked.
// Check that errors are demoted to warnings.

struct NoTemplate {
  friend void baz (int a = 1); // { dg-warning "specifies default" }
};

void baz (int a) {} // { dg-warning "only declaration" }

void ola (){
  NoTemplate t;
  baz ();
}
