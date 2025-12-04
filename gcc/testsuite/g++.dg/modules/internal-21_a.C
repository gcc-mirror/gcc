// { dg-additional-options "-fmodules" }
// { dg-module-cmi M }

export module M;

static int internal() { return 42; }

// We don't error here, despite this being an exposure we really should
// probably complain about, because we never actually expose the lambda
// (the dynamic initialization just occurs in this TU and nowhere else)
// and so this appears to function "correctly"...
export inline int x = internal(); // { dg-error "exposes TU-local" "" { xfail *-*-* } }
export inline int y
  = []{ return internal(); }(); // { dg-error "exposes TU-local" "" { xfail *-*-* } }
