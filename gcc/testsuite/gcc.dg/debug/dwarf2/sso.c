/* { dg-do compile } */
/* { dg-options "-gdwarf-3 -dA" } */

#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define REVERSE_SSO __attribute__((scalar_storage_order("big-endian")));
#else
#define REVERSE_SSO __attribute__((scalar_storage_order("little-endian")));
#endif

struct S0 { int i; };

struct S1 { int i; struct S0 s; } REVERSE_SSO;

struct S2 { int a[4]; struct S0 s; } REVERSE_SSO;

struct S0 s0;
struct S1 s1;
struct S2 s2;

/* Verify that we have endianity on the common base type of 'i' in S1 and of
   the element of 'a' in S2.  */
/* { dg-final { scan-assembler-times " DW_AT_endianity" 1 } } */
