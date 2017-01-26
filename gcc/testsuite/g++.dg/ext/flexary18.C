// PR c++/71912 - [6/7 regression] flexible array in struct in union rejected
// { dg-do compile }
// { dg-additional-options "-Wpedantic -Wno-error=pedantic" }

#if __cplusplus

namespace pr71912 {

#endif

struct foo {
  int a;
  char s[];                             // { dg-message "array member .char pr71912::foo::s \\\[\\\]. declared here" }
};

struct bar {
  double d;
  char t[];
};

struct baz {
  union {
    struct foo f;
    struct bar b;
  }
  // The definition of struct foo is fine but the use of struct foo
  // in the definition of u below is what's invalid and must be clearly
  // diagnosed.
    u;                                  // { dg-warning "invalid use of .struct pr71912::foo. with a flexible array member in .struct pr71912::baz." }
};

struct xyyzy {
  union {
    struct {
      int a;
      char s[];                         // { dg-message "declared here" }
    } f;
    struct {
      double d;
      char t[];
    } b;
  } u;                                  // { dg-warning "invalid use" }
};

struct baz b;
struct xyyzy x;

#if __cplusplus

}

#endif

// The following definitions aren't strictly valid but, like those above,
// are accepted for compatibility with GCC (in C mode).  They are benign
// in that the flexible array member is at the highest offset within
// the outermost type and doesn't overlap with other members except for
// those of the union.
union UnionStruct1 {
  struct { int n1, a[]; } s;
  int n2;
};

union UnionStruct2 {
  struct { int n1, a1[]; } s1;
  struct { int n2, a2[]; } s2;
  int n3;
};

union UnionStruct3 {
  struct { int n1, a1[]; } s1;
  struct { double n2, a2[]; } s2;
  char n3;
};

union UnionStruct4 {
  struct { int n1, a1[]; } s1;
  struct { struct { int n2, a2[]; } s2; } s3;
  char n3;
};

union UnionStruct5 {
  struct { struct { int n1, a1[]; } s1; } s2;   // { dg-warning "invalid use" }
  struct { double n2, a2[]; } s3;
  char n3;
};

union UnionStruct6 {
  struct { struct { int n1, a1[]; } s1; } s2;   // { dg-warning "invalid use" }
  struct { struct { int n2, a2[]; } s3; } s4;
  char n3;
};

union UnionStruct7 {
  struct { int n1, a1[]; } s1;
  struct { double n2, a2[]; } s2;
  struct { struct { int n3, a3[]; } s3; } s4;
};

union UnionStruct8 {
  struct { int n1, a1[]; } s1;
  struct { struct { int n2, a2[]; } s2; } s3;
  struct { struct { int n3, a3[]; } s4; } s5;
};

union UnionStruct9 {
  struct { struct { int n1, a1[]; } s1; } s2;   // { dg-warning "invalid use" }
  struct { struct { int n2, a2[]; } s3; } s4;
  struct { struct { int n3, a3[]; } s5; } s6;
};

struct StructUnion1 {
  union {
    struct { int n1, a1[]; } s1;        // { dg-message "declared here" }
    struct { double n2, a2[]; } s2;
    char n3;
  } u;                                  // { dg-warning "invalid use" }
};

// The following are invalid and rejected.
struct StructUnion2 {
  union {
    struct { int n1, a1[]; } s1;        // { dg-error "not at end" }
  } u;
  char n3;                              // { dg-message "next member" }
};

struct StructUnion3 {
  union {
    struct { int n1, a1[]; } s1;        // { dg-error "not at end" }
    struct { double n2, a2[]; } s2;
  } u;
  char n3;                              // { dg-message "next member" }
};

struct StructUnion4 {
  union {
    struct { int n1, a1[]; } s1;        // { dg-error "not at end" }
  } u1;
  union {
    struct { double n2, a2[]; } s2;
  } u2;                                 // { dg-message "next member" }
};

struct StructUnion5 {
  union {
    union {
      struct { int n1, a1[]; } s1;      // { dg-message "declared here" }
    } u1;
    union { struct { int n2, a2[]; } s2; } u2;
  } u;                                  // { dg-warning "invalid use" }
};

struct StructUnion6 {
  union {
    struct { int n1, a1[]; } s1;        // { dg-message "declared here" }
    union { struct { int n2, a2[]; } s2; } u2;
  } u;                                  // { dg-warning "invalid use" }
};

struct StructUnion7 {
  union {
    union {
      struct { double n2, a2[]; } s2;   // { dg-message "declared here" }
    } u2;
    struct { int n1, a1[]; } s1;
  } u;                                  // { dg-warning "invalid use" }
};

struct StructUnion8 {
  struct {
    union {
      union {
	struct { int n1, a1[]; } s1;    // { dg-error "not at end" }
      } u1;
      union {
	struct { double n2, a2[]; } s2;
      } u2;
    } u;
  } s1;

  struct {
    union {
      union {
	struct { int n1, a1[]; } s1;
      } u1;
      union {
	struct { double n2, a2[]; } s2;
      } u2;
    } u; } s2;                              // { dg-message "next member" }
};

struct StructUnion9 {                       // { dg-message "in the definition" }
  struct A1 {
    union B1 {
      union C1 {
	struct Sx1 { int n1, a1[]; } sx1;   // { dg-error "not at end" }
      } c1;
      union D1 {
	struct Sx2 { double n2, a2[]; } sx2;
      } d1;
    } b1;                                   // { dg-warning "invalid use" }
  } a1;

  struct A2 {
    union B2 {
      union C2 {
	struct Sx3 { int n3, a3[]; } sx3;   // { dg-message "declared here" }
      } c2;
      union D2 { struct Sx4 { double n4, a4[]; } sx4; } d2;
    } b2;                                   // { dg-warning "invalid use" }
  } a2;                                     // { dg-message "next member" }
};

// { dg-prune-output "forbids flexible array member" }
