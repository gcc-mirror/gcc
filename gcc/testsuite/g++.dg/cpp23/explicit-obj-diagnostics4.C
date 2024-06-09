// P0847R7
// { dg-do compile { target c++23 } }

// location diagnostic text when an error is emitted from an xobj member function
// this does not test for specific ill-formed code, just the additional diagnostic message

// { dg-message "In explicit object member function" "" { target *-*-* } 0 }

struct S {
  void f(this S s) {
    // The specific diagnosis issued here does not matter
    // we just need to force an error to be emitted
    +s; // { dg-error "" }
  }
};

