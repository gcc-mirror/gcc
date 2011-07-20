// PR c++/6709 (DR 743)
// PR c++/42603 (DR 950)
// { dg-options -std=c++0x }

template <class T>
T make();

struct p { typedef int t; };
struct c : decltype(make<p>()) {};

decltype(make<p>())::t t;

int f();
decltype(f())::t t2;		// { dg-error "not a class" }

struct D: decltype(f()) { };	// { dg-error "not a class" }

// { dg-prune-output "expected initializer" }
