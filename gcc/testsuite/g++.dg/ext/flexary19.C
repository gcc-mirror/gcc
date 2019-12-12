// { dg-do compile }
// { dg-additional-options "-Wpedantic -Wno-error=pedantic" }

// Verify that flexible array members are recognized as either valid
// or invalid in anonymous structs (a G++ extension) and C++ anonymous
// unions as well as in structs and unions that look anonymous but
// aren't.
struct S1
{
  int i;

  // The following declares a named data member of an unnamed struct
  // (i.e., it is not an anonymous struct).
  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s;
};

struct S2
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s[1];
};

struct S3
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s[];
};

struct S4
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s[2];
};

struct S5
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s[1][2];
};

struct S6
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s[][2];
};

struct S7
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } *s;
};

struct S8
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } **s;
};

struct S9
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } *s[1];
};

struct S10
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } *s[];
};

struct S11
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } **s[1];
};

struct S12
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } **s[];
};

struct S13
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } **s[2];
};

struct S14
{
  int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } &s;
};

struct S15
{
  int i;

  typedef struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } T15;
};

struct S16
{
  int i;

  struct {          // { dg-warning "10:ISO C\\+\\+ prohibits anonymous struct|invalid use" }
    // A flexible array as a sole member of an anonymous struct is
    // rejected with an error in C mode but emits just a pedantic
    // warning in C++.  Other than excessive pedantry there is no
    // reason to reject it.
    int a[];
  };
};

struct S17
{
  int i;

  union {           // anonymous union
    int a[];        // { dg-error "flexible array member in union" }
  };
};

struct S18
{
  int i;

  struct {
    int j, a[];     // { dg-message "declared here" }
  } s;              // { dg-warning "invalid use" }
};

struct S19
{
  int i;

  struct {          // { dg-warning "10:ISO C\\+\\+ prohibits anonymous struct|invalid use" }
    int j, a[];     // { dg-message "declared here" }
  };
};

struct S20
{
  static int i;
  typedef int A[];

  struct {
    int j;
    A a;            // { dg-message "declared here" }
  } s;              // { dg-warning "invalid use" }
};

struct S21
{
  static int i;
  typedef int A[];

  struct {          // { dg-warning "10:ISO C\\+\\+ prohibits anonymous struct|invalid use" }
    int j;
    A a;            // { dg-message "declared here" }
  };
};

struct S22
{
  struct S22S {
    static int i;

    int a[];        // { dg-error "in an otherwise empty" }
  } s;
};

struct S23
{
  struct {          // { dg-warning "10:ISO C\\+\\+ prohibits anonymous struct" }
    static int i;   // { dg-error "static data member" }

    int a[];        // { dg-error "in an otherwise empty" }
  };
};

struct S24
{
  static int i;

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s;
};

struct S25
{
  int i;

  struct {
    int j, a[];     // { dg-message "declared here" }
  } s;              // { dg-warning "invalid use" }

  // Verify that a static data member of the enclosing class doesn't
  // cause infinite recursion or some such badness.
  static S25 s2;
};

struct S26
{
  template <class>
  struct S26S {
    static int a;
  };

  struct {
    int a[];        // { dg-error "in an otherwise empty" }
  } s;
};

struct S27
{
  S27 *p;
  int a[];
};

struct S28
{
  struct A {
    struct B {
      S28 *ps28;
      A   *pa;
      B   *pb;
    } b, *pb;
    A *pa;
  } a, *pa;

  S28::A *pa2;
  S28::A::B *pb;

  int flexarray[];
};

// Verify that the notes printed along with the warnings point to the types
// or members they should point to and mention the correct relationships
// with the flexible array members.
namespace Notes
{
union A
{
  struct {
    struct {
      int i, a[];   // { dg-message "declared here" }
    } c;            // { dg-warning "invalid use" }
  } d;
  int j;
};

union B
{
  struct {          // { dg-warning "10:ISO C\\+\\+ prohibits anonymous struct" }
    struct {        // { dg-warning "12:ISO C\\+\\+ prohibits anonymous struct|invalid use" }
      int i, a[];   // { dg-message "declared here" }
    };
  };
  int j;
};

}

typedef struct Opaque* P29;
struct S30 { P29 p; };
struct S31 { S30 s; };

typedef struct { } S32;
typedef struct { S32 *ps32; } S33;
typedef struct
{
  S33 *ps33;
} S34;

struct S35
{
  struct A {
    int i1, a1[];
  };

  struct B {
    int i2, a2[];
  };

  typedef struct {
    int i3, a3[];
  } C;

  typedef struct {
    int i4, a4[];
  } D;

  typedef A A2;
  typedef B B2;
  typedef C C2;
  typedef D D2;
};

// { dg-prune-output "forbids flexible array member" }
