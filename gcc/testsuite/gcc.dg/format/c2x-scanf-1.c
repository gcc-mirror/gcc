/* Test for scanf formats.  Formats using C2X features.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic -Wformat" } */

#include "format.h"

void
foo (unsigned int *uip, unsigned short int *uhp, unsigned char *uhhp,
     unsigned long int *ulp, unsigned long long *ullp, uintmax_t *ujp,
     size_t *zp, unsigned_ptrdiff_t *utp, int_least8_t *i8, int_least16_t *i16,
     int_least32_t *i32, int_least64_t *i64, uint_least8_t *u8,
     uint_least16_t *u16, uint_least32_t *u32, uint_least64_t *u64,
     int_fast8_t *if8, int_fast16_t *if16, int_fast32_t *if32,
     int_fast64_t *if64, uint_fast8_t *uf8, uint_fast16_t *uf16,
     uint_fast32_t *uf32, uint_fast64_t *uf64)
{
  scanf ("%*b");
  scanf ("%2b", uip);
  scanf ("%hb%hhb%lb%llb%jb%zb%tb", uhp, uhhp, ulp, ullp, ujp, zp, utp);
  scanf ("%Lb", ullp); /* { dg-warning "does not support" } */
  scanf ("%qb", ullp); /* { dg-warning "does not support" } */
  /* Use of %wN and %wfN with each valid conversion specifier.  */
  scanf ("%w8d %w16d %w32d %w64d %wf8d %wf16d %wf32d %wf64d",
	 i8, i16, i32, i64, if8, if16, if32, if64);
  scanf ("%w8i %w16i %w32i %w64i %wf8i %wf16i %wf32i %wf64i",
	 i8, i16, i32, i64, if8, if16, if32, if64);
  scanf ("%w8b %w16b %w32b %w64b %wf8b %wf16b %wf32b %wf64b",
	 u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  scanf ("%w8o %w16o %w32o %w64o %wf8o %wf16o %wf32o %wf64o",
	 u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  scanf ("%w8u %w16u %w32u %w64u %wf8u %wf16u %wf32u %wf64u",
	 u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  scanf ("%w8x %w16x %w32x %w64x %wf8x %wf16x %wf32x %wf64x",
	 u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  scanf ("%w8X %w16X %w32X %w64X %wf8X %wf16X %wf32X %wf64X",
	 u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  scanf ("%w8n %w16n %w32n %w64n %wf8n %wf16n %wf32n %wf64n",
	 i8, i16, i32, i64, if8, if16, if32, if64);
  /* Use of %wN and %wfN with bad conversion specifiers.  */
  scanf ("%w8a", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16a", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32a", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64a", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8a", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16a", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32a", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64a", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8A", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16A", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32A", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64A", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8A", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16A", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32A", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64A", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8c", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16c", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32c", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64c", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8c", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16c", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32c", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64c", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8e", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16e", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32e", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64e", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8e", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16e", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32e", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64e", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8E", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16E", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32E", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64E", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8E", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16E", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32E", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64E", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8f", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16f", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32f", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64f", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8f", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16f", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32f", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64f", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8F", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16F", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32F", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64F", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8F", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16F", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32F", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64F", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8g", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16g", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32g", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64g", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8g", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16g", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32g", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64g", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8G", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16G", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32G", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64G", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8G", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16G", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32G", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64G", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8p", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16p", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32p", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64p", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8p", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16p", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32p", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64p", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8s", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16s", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32s", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64s", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8s", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16s", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32s", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64s", if64); /* { dg-warning "length modifier" } */
  scanf ("%w8[abc]", i8); /* { dg-warning "length modifier" } */
  scanf ("%w16[abc]", i16); /* { dg-warning "length modifier" } */
  scanf ("%w32[abc]", i32); /* { dg-warning "length modifier" } */
  scanf ("%w64[abc]", i64); /* { dg-warning "length modifier" } */
  scanf ("%wf8[abc]", if8); /* { dg-warning "length modifier" } */
  scanf ("%wf16[abc]", if16); /* { dg-warning "length modifier" } */
  scanf ("%wf32[abc]", if32); /* { dg-warning "length modifier" } */
  scanf ("%wf64[abc]", if64); /* { dg-warning "length modifier" } */
}
