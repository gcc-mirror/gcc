// { dg-do assemble  }
// GROUPS uncaught
// Cfront bug A.3 (See Language System Release Notes for the
// SPARCompiler C++ version 3.0)

struct S1 {
  static int S1;		// { dg-error "" } uses same name 9.3
};
struct S2 {
  union { int ii; float S2; };	// { dg-error "" } uses same name 9.3
};
