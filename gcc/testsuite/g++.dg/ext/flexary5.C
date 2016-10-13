// { dg-do compile }
// { dg-options "-Wno-error=pedantic" }

// Test to verify flexible array members handling in base and derived
// classes.

#include "flexary.h"

template <class T>
struct S_no_diag: T {
  char a[];   // cannot be diagnosed unless/until T is known
};

template <class T>
struct STx_1: T {
  char a[];   // { dg-error "flexible array member" }
};

template <class T, int I>
struct STI: T {
  char a[I];   // cannot be diagnosed unless/until T and I are known
};

template <class T, int I>
struct STIx: T {
  char a[I];
};

template <int> struct E { };

STx_1<E<0> > stx_empty_1;
STIx<E<0>, 0> stix_empty_1;

// Verify that a sole flexible array member in a class with all empty
// base classes is diagnosed.
struct E1: E<0>, E<1> { };
struct E2: E<2>, E<3> { };
struct D1: E1, E2
{
    char a[];   // { dg-error "flexible array member" }
};

struct NE { size_t i; };

struct A1x { int n, a[]; };
struct D2: A1x, E1, E2 { };

// Verify that the offset of the flexible array member is equal
// to the size of each of the valid structs.
ASSERT_AT_END (D2, a);

struct D3: E1, A1x, E2 { };

ASSERT_AT_END (D3, a);

struct D4: E1, E2, A1x { };

ASSERT_AT_END (D4, a);

// Class with non-static data members and at least one base class
// with such a member is not a standard layout class.  The warning
// below is benign since GCC computes the expected value.
struct D5: E1, E2, NE { char a[]; };

ASSERT_AT_END (D5, a);   // { dg-warning "offsetof within non-standard-layout" }

struct A2x_1 {
  size_t n;
  size_t a[];   // { dg-error "not at end of .struct D6." }
};

struct A2x_2 {
  size_t n;
  size_t a[];   // { dg-error "not at end of .struct D7." }
};

struct A2x_3 {
  size_t n;
  size_t a[];   // { dg-error "not at end of .struct D8." }
};

// Verify that the flexible array member in A2x above is diagnosed
// for each of the three struct defintions below which also derive
// from another struct with a flexible array member.
struct D6: A2x_1, E1, A1x { };
struct D7: E1, A2x_2, E2, A1x { };
struct D8: E1, E2, A2x_3, A1x { };

struct DA2x: A2x_1 { };

struct D9: DA2x, E1, E2 { };

ASSERT_AT_END (D9, a);

struct D10: E1, DA2x, E2 { };

ASSERT_AT_END (D10, a);

struct D11: E1, E2, DA2x { };

ASSERT_AT_END (D11, a);

struct A3x {
  size_t n;
  size_t a[];   // { dg-error "not at end of .struct D12.| D13.| D14.| D15." }
};

// Verify that the flexible array member in A3x above is diagnosed
// for each of the three struct defintions below which also derive
// from another struct with a non-static member.
struct D12: A3x, E1, NE { };
struct D13: E1, A3x, NE { };
struct D14: E1, E2, A3x, NE { };
struct D15: E1, E2, NE, A3x { };

struct A4x {
  A4x ();
  ~A4x ();

  size_t n;
  struct AS {
    AS (int);
    ~AS ();
    size_t i;
  } a[];
};

struct D16: A4x, E1, E2 { };

ASSERT_AT_END (D16, a);

struct D17: E1, A4x, E2 { };

ASSERT_AT_END (D17, a);

struct D18: E1, E2, A4x { };

ASSERT_AT_END (D18, a);

struct DA4x: A4x { };

struct D19: DA4x, E1, E2 { };

ASSERT_AT_END (D19, a);

struct D20: E1, DA4x, E2 { };

ASSERT_AT_END (D20, a);

struct D21: E1, E2, DA4x { };

ASSERT_AT_END (D21, a);


struct A5x {
  A5x (int);
  virtual ~A5x ();

  size_t n;
  struct AS {
    AS (int);
    ~AS ();
    size_t i;
  } a[];
};

struct D22: A5x, E1, E2 { };

ASSERT_AT_END (D22, a);   // { dg-warning "offsetof within non-standard-layout" }

struct D23: E1, A5x, E2 { };

ASSERT_AT_END (D23, a);   // { dg-warning "offsetof within non-standard-layout" }

struct D24: E1, E2, A5x { };

ASSERT_AT_END (D24, a);   // { dg-warning "offsetof within non-standard-layout" }

struct DA5x: A5x { };

struct D25: DA5x, E1, E2 { };

ASSERT_AT_END (D25, a);   // { dg-warning "offsetof within non-standard-layout" }

struct D26: E1, DA5x, E2 { };

ASSERT_AT_END (D26, a);   // { dg-warning "offsetof within non-standard-layout" }

struct D27: E1, E2, DA5x { };

ASSERT_AT_END (D27, a);   // { dg-warning "offsetof within non-standard-layout" }

// Verfify that a flexible array member is diagnosed even when deep
// in the base class hierarchy.
struct A6x {
  size_t n;
  size_t a[];               // { dg-error "not at end of .struct D28.| D29." }
};

struct AA6x: A6x { };
struct NE1: NE { };
struct NE2: NE { };

struct D28: NE1, AA6x { };
struct D29: AA6x, NE1 { };

struct A7x {
  size_t n;
  size_t a[];               // { dg-error "flexible array member .A7x::a. not at end of .struct D33." }
};

// Verify that a flexible array member in a virtual base class is not
// diagnosed.
struct DA7xV1: virtual A7x { };
struct DA7xV2: virtual A7x { };

struct D30: DA7xV1, DA7xV2 { };
struct D31: DA7xV1, DA7xV2 { };
struct D32: D30, D31 { };

// Verify the diagnostic when the flexible array is in an anonymous struct.
struct A8x {
  struct {                  // { dg-message "next member .A8x::<unnamed struct> A8x::<anonymous>. declared here" }
    size_t n;
    size_t a[];
  };
};

struct D33:                 // { dg-message "in the definition of .struct D33." }
  A7x, A8x { };
