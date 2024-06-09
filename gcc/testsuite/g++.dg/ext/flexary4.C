// PR c++/42121 - g++ should warn or error on internal 0 size array in struct
// { dg-do compile }
// { dg-options "-Wno-error=pedantic" }

// Flexible array members are a feature of C99 (and newer) not provided
// by C++ 2014 and prior.  G++ supports both the C99/C11 kind of flexible
// array members and pre-C99 zero-size arrays (defining an array of size
// zero).  Since both features are provided for compatibility with C,
// G++ allows them in the same contexts as in C.

#include "flexary.h"

struct Sx {
  int a[];
};

// Verify that non-data members or static data members either before
// or after a flexible array member in an otherwise empty struct don't
// suppress the diagnostic.
struct Sx2 {
  int a[];
  typedef int I;
};

struct Sx3 {
  typedef int I;
  int a[];
};

struct Sx4 {
  int a[];
  enum E { e };
};

struct Sx5 {
  enum E { e };
  int a[];
};

struct Sx6 {
  int a[];
  static int i;
};

struct Sx7 {
  static int i;
  int a[];
};

struct Sx8 {
  int a[];
  Sx8 () { }
};

struct Sx9 {
  Sx9 () { }
  int a[];
};

struct Sx10 {
  int a[];
  virtual ~Sx10 () { }
};

struct Sx11 {
  virtual ~Sx11 () { }
  int a[];
};

struct Sx12 {
  int a[];
  virtual void foo () = 0;
};

struct Sx13 {
  virtual void foo () = 0;
  int a[];
};

struct Sx14 {
  int a[][1];
};

struct Sx15 {
  typedef int A[];
  A a;
};

// Verify also that a zero-size array doesn't suppress the diagnostic.
struct Sx16 {
  // a_0 below is diagnosed with -Wpedantic only and emits
  // warning: ISO C++ forbids zero-size arrays
  int a_0 [0];
  int a_x [];
};

struct Sx17 {
  int a_x [];               // { dg-error "flexible array member" }

  // a_0 below is diagnosed with -Wpedantic only and emits
  // warning: ISO C++ forbids zero-size arrays
  int a_0 [0];
};

// An empty struct is treated as if it had a single member of type
// char but the member cannot be accessed.  Therefore, a struct
// containing a flexible array member followed by an empty struct
// is diagnosed to prevent the former subobject from sharing space
// with the latter.
struct Sx18 {
  int a_x [];               // { dg-error "flexible array member" }
  struct { /* empty */ } s;
};

// Anonymous structs are a G++ extension.  Members of anonymous structs
// are treated as if they were declared in the enclosing class.
struct Sx19 {
  struct { int i; };        // anonymous struct
  int a_x [];
};

// Unlike in the case above, a named struct is not anonymous and
// so doesn't contribute its member to that of the enclosing struct.
struct Sx20 {
  struct S { int i; };
  int a_x [];
};

struct Sx21 {
  int a_x [];               // { dg-error "not at end" }
  struct S { } s;
};

struct Sx22 {
  int a_x [];               // { dg-error "not at end" }
  union { int i; };
};

struct Sx23 {
  union { int i; };
  int a_x [];
};

struct Sx24 {
  struct S;
  S a_x [];                 // { dg-error "5:field .a_x. has incomplete type" }
};

struct Sx25 {
  struct S { };
  S a_x [];
};

struct Sx26 {
  struct { }
    a_x [];
};

struct Sx27 {
  int i;
  struct { }
    a_x [];
};

ASSERT_AT_END (Sx27, a_x);

struct Sx28 {
  struct { }
    a_x [];                   // { dg-error "not at end" }
  int i;
};

struct Sx29 {
  // Pointer to an array of unknown size.
  int (*a_x)[];
};

struct Sx30 {
  // Reference to an array of unknown size.
  int (&a_x)[];
};

struct Sx31 {
  int a [];                 // { dg-error "not at end" }
  unsigned i: 1;
};

struct Sx32 {
  unsigned i: 1;
  int a [];
};

ASSERT_AT_END (Sx32, a);

struct Sx33 {
  int a [];
  friend int foo ();
};

struct Sx34 {
  friend int foo ();
  int a [];
};

// Verify that intervening non-field declarations of members other
// than non-static data members don't affect the diagnostics.
struct Sx35 {
  int a[];                  // { dg-error "not at end" }
  typedef int I;
  int n;
};

struct Sx36 {
  int n;
  typedef int I;
  int a[];
};

ASSERT_AT_END (Sx36, a);

struct Sx37 {
  int a[];                  // { dg-error "not at end" }
  enum E { };
  int n;
};

struct Sx38 {
  int n;
  enum E { };
  int a[];
};

ASSERT_AT_END (Sx38, a);

struct Sx39 {
  int a[];                  // { dg-error "not at end" }
  struct S;
  int n;
};

struct Sx40 {
  int n;
  struct S;
  int a[];
};

ASSERT_AT_END (Sx40, a);

struct Sx41 {
  int a[];                  // { dg-error "not at end" }
  static int i;
  int n;
};

struct Sx42 {
  int n;
  static int i;
  int a[];
};

ASSERT_AT_END (Sx42, a);

struct Sx43 {
  int a[];                  // { dg-error "not at end" }
  Sx43 ();
  int n;
};

struct Sx44 {
  int n;
  Sx44 ();
  int a[];
};

ASSERT_AT_END (Sx44, a);

struct S_S_S_x {
  struct A {
    struct B {
      int a[];
    } b;
  } a;
};

// Since members of an anonymous struct or union are considered to be
// members of the enclosing union the below defintions are valid and
// must be accepted.

struct Anon1 {
  int n;
  struct {
    int good[];
  };
};

ASSERT_AT_END (Anon1, good);

struct NotAnon1 {
  int n;
  // The following is not an anonymous struct -- the type is unnamed
  // but the object has a name.
  struct {
    int bad[];
  } name;
};

struct Anon2 {
  struct {
    int n;
    struct {
      int good[];
    };
  };
};

ASSERT_AT_END (Anon2, good);

struct Anon3 {
  struct {
    struct {
      int n;
      int good[];
    };
  };
};

ASSERT_AT_END (Anon3, good);

struct Anon4 {
  struct {
    int in_empty_struct[];
  };
};

struct Anon5 {
  struct {
    int not_at_end[];       // { dg-error "not at end" }
  };
  int n;
};

struct Anon6 {
  struct {
    struct {
      int not_at_end[];     // { dg-error "not at end" }
    };
    int n;
  };
};


struct Anon7 {
  struct {
    struct {
      int not_at_end[];     // { dg-error "not at end" }
    };
  };
  int n;
};

struct Six {
  int i;
  int a[];
};

ASSERT_AT_END (Six, a);

class Cx {
  int a[];
};

class Cix {
  int i;
  int a[];
};

struct Sxi {
  int a[];                  // { dg-error "not at end" }
  int i;
};

struct S0 {
  int a[0];
};

struct S0i {
  int a[0];
  int i;
};

struct S_a0_ax {
  int a0[0];
  int ax[];
};

struct S_a0_i_ax {
  int a0[0];
  int i;
  int ax[];
};

ASSERT_AT_END (S_a0_i_ax, ax);

struct Si_a0_ax {
  int i;
  int a0[0];
  int ax[];
};

ASSERT_AT_END (Si_a0_ax, ax);

struct Si_ax_a0 {
  int i;
  int ax[];                 // { dg-error "not at end" }
  int a0[0];
};

struct S_u0_ax {
  union { } u[0];
  int ax[];
};

struct S_a1_s2 {
  int a[1];
  int b[2];
};
