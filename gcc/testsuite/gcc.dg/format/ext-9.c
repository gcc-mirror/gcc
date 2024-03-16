/* Test for printf format extensions using formats from or recommended by
   C23.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu23 -Wformat" } */

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
}
