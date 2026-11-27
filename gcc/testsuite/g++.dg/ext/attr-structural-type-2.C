// C++26 P3861R1 - Pragmatic approach to standard structural types
// { dg-do compile { target c++11 } }
// Negative tests.

struct [[gnu::structural_type]] A {
 [[gnu::structural_type]] int &foo (int &);	// { dg-warning "'structural_type' attribute can only be applied to class types" }
};

[[gnu::structural_type]] int &bar (int &);	// { dg-warning "'structural_type' attribute can only be applied to class types" }

[[gnu::structural_type]] int i;			// { dg-warning "'structural_type' attribute can only be applied to class types" }
[[gnu::structural_type]] double d;		// { dg-warning "'structural_type' attribute can only be applied to class types" }
[[gnu::structural_type]] typedef int T;		// { dg-warning "'structural_type' attribute can only be applied to class types" }

[[gnu::structural_type()]] int &fn1 (int &);	// { dg-error "'structural_type' attribute does not take any arguments" }
[[gnu::structural_type("a")]] int &fn2 (int &); // { dg-error "'structural_type' attribute does not take any arguments" }
[[gnu::structural_type(true, true)]] int &fn3 (int &); // { dg-error "'structural_type' attribute does not take any arguments" }

enum [[gnu::structural_type]] E {		// { dg-warning "'structural_type' attribute can only be applied to class types" }
  X [[gnu::structural_type]]			// { dg-warning "'structural_type' attribute can only be applied to class types" }
};

[[gnu::structural_type]];			// { dg-warning "ignored" }

void
g ()
{
  goto L;
[[gnu::structural_type]] L:;			// { dg-warning "'structural_type' attribute can only be applied to class types" }
}
