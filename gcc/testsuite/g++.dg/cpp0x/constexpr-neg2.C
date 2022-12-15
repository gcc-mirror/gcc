// PR c++/54020
// { dg-do compile { target c++11 } }

// Preliminaries.
extern int nonconst_func(int);
constexpr int identity(int x) { return x; }
constexpr int zero() { return identity(0); }
constexpr int one() { return identity(1); }

// Correctly accepted.
constexpr int three = one() ? 3 : nonconst_func(0);

// Incorrectly accepted.  See [dcl.constexpr] #5:
//   For a constexpr function, if no function argument values exist
//   such that the function invocation sub-stitution would produce a
//   constant expression (5.19), the program is ill-formed; no diagnostic
//   required.
constexpr int bogus() { return zero () ? 3 : nonconst_func(0); } // { dg-error "nonconst_func" }

// Correctly rejected (not sure why).
constexpr int correct_error() { return nonconst_func(0); } // { dg-error "nonconst_func" "" { target c++20_down } }

// Correctly rejected.
constexpr int z = bogus();	// { dg-error "" }

// This is also correctly rejected.
constexpr int correct_failure() { return 0 ? 3 : nonconst_func(0); } // { dg-error "nonconst_func" "" { target c++20_down } }
