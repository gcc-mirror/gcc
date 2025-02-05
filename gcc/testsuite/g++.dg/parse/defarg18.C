// PR c++/118319
// { dg-do "compile" { target c++11 } }

// Template case, that used to crash.

template <int>
struct S {
  friend void foo1 (int a = []{}());  // { dg-error "specifies default|only declaration" }
  friend void foo3 (int a,	      // { dg-error "specifies default|only declaration" }
		    int b = []{}(),
		    int c = []{}());
};

void foo1 (int a) {}
void foo3 (int a, int b, int c) {}

void hello (){
  S<0> t;
  foo1 ();
  foo3 (1, 2);
}


// Template case, that already worked.

template <int>
struct T {
  friend void bar (int a = []{}()); // { dg-error "specifies default|only declaration" }
};

void hallo (){
  T<0> t;
  bar (); // { dg-error "not declared" }
}


// Non template case, that already worked.

struct NoTemplate {
  friend void baz (int a = []{}()); // { dg-error "specifies default|could not convert" }
};

void baz (int a) {} // { dg-error "only declaration" }

void ola (){
  NoTemplate t;
  baz (); // { dg-error "void value not ignored" }
}
