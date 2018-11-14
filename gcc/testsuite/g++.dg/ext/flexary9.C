// { dg-do compile }
// { dg-options "-Wpedantic -Wno-error=pedantic" }

#include "flexary.h"

struct Sx {
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
};

// Verify that non-data members or static data members either before
// or after a zero-length array in an otherwise empty struct don't
// suppress the diagnostic.
struct Sx2 {
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
  typedef int I;
};

struct Sx3 {
  typedef int I;
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
};

struct Sx4 {
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
  enum E { e };
};

struct Sx5 {
  enum E { e };
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
};

struct Sx6 {
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
  static int i;
};

struct Sx7 {
  static int i;
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
};

struct Sx8 {
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
  Sx8 () { }
};

struct Sx9 {
  Sx9 () { }
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
};

struct Sx10 {
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
  virtual ~Sx10 () { }
};

struct Sx11 {
  virtual ~Sx11 () { }
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
};

struct Sx12 {
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
  virtual void foo () = 0;
};

struct Sx13 {
  virtual void foo () = 0;
  int a[0];                 // { dg-warning "zero-size|in an otherwise empty" }
};

struct Sx14 {
  int a[0][1];             // { dg-warning "zero-size|in an otherwise empty" }
};

struct Sx15 {
  typedef int A[0];         // { dg-warning "zero-size" }
  A a;                      // { dg-warning "in an otherwise empty" }
};

// Verify also that a zero-size array doesn't suppress the diagnostic.
struct Sx16 {
  int a_0 [0];              // { dg-warning "zero-size|in an otherwise empty" }
  int a_x [0];              // { dg-warning "zero-size array" }
};

struct Sx17 {
  int a_x [0];              // { dg-warning "zero-size|in an otherwise empty" }
  int a_0 [0];              // { dg-warning "zero-size array" }
};

// Empty structs are a GCC extension that (in C++ only) is treated
// as if it had a single member of type char.  Therefore, a struct
// containing a zero-length array followed by an empty struct
// is diagnosed to prevent the former subobject from sharing space
// with the latter.
struct Sx18 {
  int a_x [0];              // { dg-warning "zero-size array" }
  struct S { };
};

// Anonymous structs and unions are another GCC extension.  Since
// they cannot be named and thus used to store the size of a zero
// length array member, a struct containing both is diagnosed as
// if the zero-length array appeared alone.
struct Sx19 {
  struct S { };
  union U { };
  int a_x [0];              // { dg-warning "zero-size|in an otherwise empty" }
};

// Unlike in the case above, a named member of an anonymous struct
// prevents a subsequent zero-length array from being diagnosed.
struct Sx20 {
  struct S { } s;
  int a_x [0];              // { dg-warning "zero-size array" }
};

struct Sx21 {
  int a_x [0];              // { dg-warning "zero-size array|not at end" }
  struct S { } s;
};

struct Sx22 {
  int a_x [0];              // { dg-warning "zero-size array|not at end" }
  union { int i; };
};

struct Sx23 {
  union { int i; };
  int a_x [0];              // { dg-warning "zero-size array" }
};

// The following causes an incomplete type error error and a zero-size
// array warning.
struct Sx24 {
  struct S;
  S a_x [0];                // { dg-error "5:field .a_x. has incomplete type" }
// { dg-warning "zero-size array" "" { target *-*-* } .-1 }
};

struct Sx25 {
  struct S { };
  S a_x [0];                // { dg-warning "zero-size array" }
};

struct Sx26 {
  struct { }
    a_x [0];                // { dg-warning "zero-size array" }
};

struct Sx27 {
  int i;
  struct { }
    a_x [0];                // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Sx27, a_x);

struct Sx28 {
  struct { }
    a_x [0];                // { dg-warning "zero-size array|not at end" }
  int i;
};

struct Sx29 {
  // Pointer to an array of zero size.
  int (*a_x)[0];            // { dg-warning "zero-size array" }
};

struct Sx30 {
  // Reference to an array of zero size.
  int (&a_x)[0];            // { dg-warning "zero-size array" }
};

struct Sx31 {
  int a [0];                // { dg-warning "zero-size array|not at end" }
  unsigned i: 1;
};

struct Sx32 {
  unsigned i: 1;
  int a [0];                // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Sx32, a);

struct Sx33 {
  int a [0];                // { dg-warning "zero-size array|otherwise empty" }
  friend int foo ();
};

struct Sx34 {
  friend int foo ();
  int a [0];                // { dg-warning "zero-size array|otherwise empty" }
};

// Verify that intervening non-field declarations of members other
// than non-static data members don't affect the diagnostics.
struct Sx35 {
  int a[0];                 // { dg-warning "zero-size array|not at end" }
  typedef int I;
  int n;
};

struct Sx36 {
  int n;
  typedef int I;
  int a[0];                 // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Sx36, a);

struct Sx37 {
  int a[0];                 // { dg-warning "zero-size array|not at end" }
  enum E { };
  int n;
};

struct Sx38 {
  int n;
  enum E { };
  int a[0];                 // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Sx38, a);

struct Sx39 {
  int a[0];                 // { dg-warning "zero-size array|not at end" }
  struct S;
  int n;
};

struct Sx40 {
  int n;
  struct S;
  int a[0];                 // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Sx40, a);

struct Sx41 {
  int a[0];                 // { dg-warning "zero-size array|not at end" }
  static int i;
  int n;
};

struct Sx42 {
  int n;
  static int i;
  int a[0];                 // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Sx42, a);

struct Sx43 {
  int a[0];                 // { dg-warning "zero-size array|not at end" }
  Sx43 ();
  int n;
};

struct Sx44 {
  int n;
  Sx44 ();
  int a[0];                 // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Sx44, a);

struct S_S_S_x {
  struct A {
    struct B {
      int a[0];             // { dg-warning "zero-size array" }
    } b;
  } a;
};

// Since members of an anonymous struct or union are considered to be
// members of the enclosing union the below defintions are valid and
// must be accepted.

struct Anon1 {
  int n;
  struct {                  // { dg-warning "invalid use \[^\n\r\]* with a zero-size array" }
    int good[0];            // { dg-warning "forbids zero-size array" }
  };                        // { dg-warning "anonymous struct" }
};

ASSERT_AT_END (Anon1, good);

struct Anon2 {
  struct {                  // { dg-warning "invalid use" }
    int n;
    struct {
      int good[0];          // { dg-warning "zero-size array" }
    };                      // { dg-warning "anonymous struct" }
  };                        // { dg-warning "anonymous struct" }
};

ASSERT_AT_END (Anon2, good);

struct Anon3 {
  struct {                  // { dg-warning "invalid use" }
    struct {
      int n;
      int good[0];          // { dg-warning "zero-size array" }
    };                      // { dg-warning "anonymous struct" }
  };                        // { dg-warning "anonymous struct" }
};

ASSERT_AT_END (Anon3, good);

struct Anon4 {
  struct {
    int in_empty_struct[0]; // { dg-warning "zero-size array|in an otherwise empty" }
  };                        // { dg-warning "anonymous struct" }
};

struct Anon5 {
  struct {
    int not_at_end[0];      // { dg-warning "zero-size array|not at end" }
  };                        // { dg-warning "anonymous struct" }
  int n;
};

struct Anon6 {
  struct {
    struct {
      int not_at_end[0];    // { dg-warning "zero-size array|not at end" }
    };                      // { dg-warning "anonymous struct" }
    int n;
  };                        // { dg-warning "anonymous struct" }
};


struct Anon7 {
  struct {
    struct {
      int not_at_end[0];    // { dg-warning "zero-size array|not at end" }
    };                      // { dg-warning "anonymous struct" }
  };                        // { dg-warning "anonymous struct" }
  int n;
};


struct Six {
  int i;
  int a[0];                 // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Six, a);

class Cx {
  int a[0];                 // { dg-warning "zero-size array" }
};

class Cix {
  int i;
  int a[0];                 // { dg-warning "zero-size array" }
};

struct Sxi {
  int a[0];                 // { dg-warning "zero-size array|not at end" }
  int i;
};

struct S0 {
  int a[0];                 // { dg-warning "zero-size array" }
};

struct S0i {
  int a[0];                 // { dg-warning "zero-size array" }
  int i;
};

struct S_a0_ax {
  int a1[0];                // { dg-warning "zero-size array" }
  int ax[0];                // { dg-warning "zero-size array" }
};

struct S_a0_i_ax {
  int a1[0];                // { dg-warning "zero-size array" }
  int i;
  int ax[0];                // { dg-warning "zero-size array" }
};

ASSERT_AT_END (S_a0_i_ax, ax);

struct Si_a0_ax {
  int i;
  int a1[0];                // { dg-warning "zero-size array" }
  int ax[0];                // { dg-warning "zero-size array" }
};

ASSERT_AT_END (Si_a0_ax, ax);

struct S_u0_ax {
  union { } u[0];           // { dg-warning "zero-size array" }
  int ax[0];                // { dg-warning "zero-size array" }
};

struct S_a1_s2 {
  int a[1];
  int b[2];
};
