/* Test for printf format extensions using formats from or recommended by
   C2X.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2x -Wformat" } */

#include "format.h"

void
foo (u_quad_t uq, unsigned int u, unsigned short us, unsigned char uc,
     unsigned long ul, unsigned long long ull, uintmax_t uj, size_t z,
     unsigned_ptrdiff_t ut, int_least8_t i8, int_least16_t i16,
     int_least32_t i32, int_least64_t i64, uint_least8_t u8,
     uint_least16_t u16, uint_least32_t u32, uint_least64_t u64,
     int_fast8_t if8, int_fast16_t if16, int_fast32_t if32, int_fast64_t if64,
     uint_fast8_t uf8, uint_fast16_t uf16, uint_fast32_t uf32,
     uint_fast64_t uf64)
{
  /* Deprecated length modifiers with %b and %B.  */
  printf ("%qb%qB", uq, uq);
  printf ("%Lb%LB", ull, ull);
  printf ("%Zb%ZB", z, z);
  /* Use of %B in cases valid for %b.  */
  printf ("%B %hB %hhB %lB %llB %jB %zB %tB\n", u, us, uc, ul, ull, uj, z, ut);
  printf ("%*.*llB\n", 1, 2, ull);
  printf ("%-B\n", u);
  printf ("%#B\n", u);
  printf ("%08B\n", u);
  /* Flags valid on signed conversions only.  */
  printf ("%+B\n", u); /* { dg-warning "flag" } */
  printf ("% B\n", u); /* { dg-warning "flag" } */
  /* Flags ignored in certain combinations.  */
  printf ("%-08B\n", u); /* { dg-warning "ignored" } */
  printf ("%08.5B\n", u); /* { dg-warning "ignored" } */
  /* Use of %wN and %wfN with %B.  */
  printf ("%w8B %w16B %w32B %w64B %wf8B %wf16B %wf32B %wf64B",
	  u8, u16, u32, u64, uf8, uf16, uf32, uf64);
}
