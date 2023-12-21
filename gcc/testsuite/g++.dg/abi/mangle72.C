// Verify manglinng of class literals with pointers to members.
// Some of the mangling here is wrong.  Note the FIXME comments below.
// { dg-do compile { target c++2a } }
// { dg-additional-options -fabi-compat-version=0 }

struct A { int a[2]; };

template <A> struct X { };

// Let's mangle some non-member pointer literals for comparison.
void f__ (X<A{{ }}>) { }
// { dg-final { scan-assembler "_Z3f001XIXtl1AEEE" } }

void f0_ (X<A{{ 0 }}>) { }
// { dg-final { scan-assembler "_Z3f0_1XIXtl1AEEE" } }

void f00 (X<A{{ 0, 0 }}>) { }
// { dg-final { scan-assembler "_Z3f__1XIXtl1AEEE" } }


// Exercise arrays of pointers to data members.
typedef int (A::*padm_t)[2];

struct B { padm_t a[2]; };
template <B> struct Y { };

void g__ (Y<B{{ }}>) { }
// { dg-final { scan-assembler "_Z3g__1YIXtl1BEEE" } }

void g0_ (Y<B{{ 0 }}>) { }
// { dg-final { scan-assembler "_Z3g0_1YIXtl1BEEE" } }

void g00 (Y<B{{ 0, 0 }}>) { }
// { dg-final { scan-assembler "_Z3g001YIXtl1BEEE" } }

void g0x (Y<B{{ 0, &A::a }}>) { }
// { dg-final { scan-assembler "_Z3g0x1YIXtl1BtlA2_M1AA2_iLS3_0EadL_ZNS1_1aEEEEEE" } }

void gx_ (Y<B{{ &A::a }}>) { }
// { dg-final { scan-assembler "_Z3gx_1YIXtl1BtlA2_M1AA2_iadL_ZNS1_1aEEEEEE" } }


struct C { padm_t a[3]; };
template <C> struct Z { };

void h___ (Z<C{{ }}>) { }
// { dg-final { scan-assembler "_Z4h___1ZIXtl1CEEE" } }

void h0__ (Z<C{{ 0 }}>) { }
// { dg-final { scan-assembler "_Z4h0__1ZIXtl1CEEE" } }

void h00_ (Z<C{{ 0, 0 }}>) { }
// { dg-final { scan-assembler "_Z4h00_1ZIXtl1CEEE" } }

void h000 (Z<C{{ 0, 0, 0 }}>) { }
// { dg-final { scan-assembler "_Z4h0001ZIXtl1CEEE" } }

void h00x (Z<C{{ 0, 0, &A::a }}>) { }
// { dg-final { scan-assembler "_Z4h00x1ZIXtl1CtlA3_M1AA2_iLS3_0ELS3_0EadL_ZNS1_1aEEEEEE" } }

void h0x0 (Z<C{{ 0, &A::a, 0 }}>) { }
// { dg-final { scan-assembler "_Z4h0x01ZIXtl1CtlA3_M1AA2_iLS3_0EadL_ZNS1_1aEEEEEE" } }

void h0x_ (Z<C{{ 0, &A::a }}>) { }
// { dg-final { scan-assembler "_Z4h0x_1ZIXtl1CtlA3_M1AA2_iLS3_0EadL_ZNS1_1aEEEEEE" } }

void hx0_ (Z<C{{ &A::a, 0 }}>) { }
// { dg-final { scan-assembler "_Z4hx0_1ZIXtl1CtlA3_M1AA2_iadL_ZNS1_1aEEEEEE" } }

void hx__ (Z<C{{ &A::a }}>) { }
// { dg-final { scan-assembler "_Z4hx__1ZIXtl1CtlA3_M1AA2_iadL_ZNS1_1aEEEEEE" } }


// Exercise arrays of pointers to function members.

struct AF { void f (); };
typedef void (AF::*pafm_t)();

struct D { pafm_t a[2]; };
template <D> struct F { };

void k__ (F<D{{ }}>) { }
// { dg-final { scan-assembler "_Z3k__1FIXtl1DEEE" } }

void k0_ (F<D{{ 0 }}>) { }
// { dg-final { scan-assembler "_Z3k0_1FIXtl1DEEE" } }

void k00 (F<D{{ 0, 0 }}>) { }
// { dg-final { scan-assembler "_Z3k001FIXtl1DEEE" } }

void k0x (F<D{{ 0, &AF::f }}>) { }
// { dg-final { scan-assembler "_Z3k0x1FIXtl1DtlA2_M2AFFvvEtlS3_EtlS3_adL_ZNS1_1fEvEEEEEE" } }

void kx_ (F<D{{ &AF::f }}>) { }
// { dg-final { scan-assembler "_Z3kx_1FIXtl1DtlA2_M2AFFvvEtlS3_adL_ZNS1_1fEvEEEEEE" } }

void kx0 (F<D{{ &AF::f, 0 }}>) { }
// { dg-final { scan-assembler "_Z3kx01FIXtl1DtlA2_M2AFFvvEtlS3_adL_ZNS1_1fEvEEEEEE" } }

void kxx (F<D{{ &AF::f, &AF::f }}>) { }
// { dg-final { scan-assembler "_Z3kxx1FIXtl1DtlA2_M2AFFvvEtlS3_adL_ZNS1_1fEvEEtlS3_adL_ZNS1_1fEvEEEEEE" } }
