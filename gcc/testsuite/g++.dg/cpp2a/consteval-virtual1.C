// { dg-do compile { target c++20 } }

struct S {
  virtual int foo () { return 42; }		// { dg-message "overridden function is 'virtual int S::foo\\\(\\\)'" }
  consteval virtual int bar () { return 43; }	// { dg-message "overridden function is 'virtual consteval int S::bar\\\(\\\)'" }
};
struct T : public S {
  int bar () { return 44; }	// { dg-error "non-'consteval' function 'virtual int T::bar\\\(\\\)' overriding 'consteval' function" }
};
struct U : public S {
  consteval virtual int foo () { return 45; }	// { dg-error "'consteval' function 'virtual consteval int U::foo\\\(\\\)' overriding non-'consteval' function" }
};
