// P2169R4 - A nice placeholder with no name
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S {
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  int foo ();
  S () : _ (1) {}	// { dg-error "request for member '_' is ambiguous" }
  void bar () { ++_; }	// { dg-error "reference to '_' is ambiguous" }
};

int
S::foo ()
{
  int x = _;		// { dg-error "reference to '_' is ambiguous" }
  x += S::_;		// { dg-error "reference to '_' is ambiguous" }
  return x;
}

struct T {
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};
T t = { ._ = 1 };	// { dg-error "request for member '_' is ambiguous" }

auto o = __builtin_offsetof (T, _);	// { dg-error "request for member '_' is ambiguous" }
int T::* p = &T::_;	// { dg-error "reference to '_' is ambiguous" }

struct U {
  U () : _ (42) {}	// { dg-error "request for member '_' is ambiguous" }
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};

struct V {
  V ();
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};

V::V () : _(42)		// { dg-error "request for member '_' is ambiguous" }
{
}

struct A {
  int _;
  union { int _; };	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  A() : _(42) {}	// { dg-error "request for member '_' is ambiguous" }
};

struct B {
  union { int _, _; };	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  union { int _, _; };	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  B() : _(42) {}	// { dg-error "request for member '_' is ambiguous" }
};

void
bar ()
{
  union { int _;
	  int _; };	// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  _ = 42;		// { dg-error "reference to '_' is ambiguous" }
}

namespace C
{
  static union { int _ = 1; };
  static union { int _ = 2; };	// { dg-error "redeclaration of 'int _'" }
}

void
baz ()
{
  static union { int _ = 3; };
  static union { int _ = 4; };	// { dg-error "redeclaration of 'int _'" }
}				// { dg-message "static variable is not name-independent" "" { target c++26 } .-1 }

struct D {
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
};

struct E : public D {};

void
qux ()
{
  D {}._;		// { dg-error "request for member '_' is ambiguous" }
  E {}._;		// { dg-error "request for member '_' is ambiguous" }
}

struct F {
  struct _ {};
  int _;
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  void foo () { ++_; }	// { dg-error "reference to '_' is ambiguous" }
  void bar ();
};
typedef struct F::_ F_;

void
F::bar ()
{
  ++_;			// { dg-error "reference to '_' is ambiguous" }
}

struct G {
  int _ (int) { return 1; }
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  void foo () { ++_; }	// { dg-error "reference to '_' is ambiguous" }
  void bar ();
};

void
G::bar ()
{
  ++_;			// { dg-error "reference to '_' is ambiguous" }
  this->_ (0);		// { dg-error "request for member '_' is ambiguous" }
}

struct H {
  int _ (int) { return 1; }
  long _ (float) { return 2; }
  int _;		// { dg-warning "name-independent declarations only available with" "" { target c++23_down } }
  void foo () { ++_; }	// { dg-error "reference to '_' is ambiguous" }
  void bar ();
};

void
H::bar ()
{
  ++_;			// { dg-error "reference to '_' is ambiguous" }
  this->_ (0);		// { dg-error "request for member '_' is ambiguous" }
}
