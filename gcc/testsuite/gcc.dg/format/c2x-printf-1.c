/* Test for printf formats.  Formats using C2X features.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic -Wformat" } */

#include "format.h"

void
foo (unsigned int u, unsigned short us, unsigned char uc, unsigned long ul,
     unsigned long long ull, uintmax_t uj, size_t z, unsigned_ptrdiff_t ut,
     int_least8_t i8, int_least16_t i16, int_least32_t i32, int_least64_t i64,
     uint_least8_t u8, uint_least16_t u16, uint_least32_t u32,
     uint_least64_t u64, int_fast8_t if8, int_fast16_t if16, int_fast32_t if32,
     int_fast64_t if64, uint_fast8_t uf8, uint_fast16_t uf16,
     uint_fast32_t uf32, uint_fast64_t uf64)
{
  /* Use of %b with each length modifier and other valid features.  */
  printf ("%b %hb %hhb %lb %llb %jb %zb %tb\n", u, us, uc, ul, ull, uj, z, ut);
  printf ("%*.*llb\n", 1, 2, ull);
  printf ("%-b\n", u);
  printf ("%#b\n", u);
  printf ("%08b\n", u);
  /* Flags valid on signed conversions only.  */
  printf ("%+b\n", u); /* { dg-warning "flag" } */
  printf ("% b\n", u); /* { dg-warning "flag" } */
  /* Flags ignored in certain combinations.  */
  printf ("%-08b\n", u); /* { dg-warning "ignored" } */
  printf ("%08.5b\n", u); /* { dg-warning "ignored" } */
  /* Use of 'L' and 'q' for long long is an extension.  */
  printf ("%Lb", ull); /* { dg-warning "does not support" } */
  printf ("%qb", ull); /* { dg-warning "does not support" } */
  /* Use of %wN and %wfN with each valid conversion specifier.  */
  printf ("%w8d %w16d %w32d %w64d %wf8d %wf16d %wf32d %wf64d",
	  i8, i16, i32, i64, if8, if16, if32, if64);
  printf ("%w8i %w16i %w32i %w64i %wf8i %wf16i %wf32i %wf64i",
	  i8, i16, i32, i64, if8, if16, if32, if64);
  printf ("%w8b %w16b %w32b %w64b %wf8b %wf16b %wf32b %wf64b",
	  u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  printf ("%w8o %w16o %w32o %w64o %wf8o %wf16o %wf32o %wf64o",
	  u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  printf ("%w8u %w16u %w32u %w64u %wf8u %wf16u %wf32u %wf64u",
	  u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  printf ("%w8x %w16x %w32x %w64x %wf8x %wf16x %wf32x %wf64x",
	  u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  printf ("%w8X %w16X %w32X %w64X %wf8X %wf16X %wf32X %wf64X",
	  u8, u16, u32, u64, uf8, uf16, uf32, uf64);
  printf ("%w8n %w16n %w32n %w64n %wf8n %wf16n %wf32n %wf64n",
	  &i8, &i16, &i32, &i64, &if8, &if16, &if32, &if64);
  /* Use of %wN and %wfN with bad conversion specifiers.  */
  printf ("%w8a", i8); /* { dg-warning "length modifier" } */
  printf ("%w16a", i16); /* { dg-warning "length modifier" } */
  printf ("%w32a", i32); /* { dg-warning "length modifier" } */
  printf ("%w64a", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8a", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16a", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32a", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64a", if64); /* { dg-warning "length modifier" } */
  printf ("%w8A", i8); /* { dg-warning "length modifier" } */
  printf ("%w16A", i16); /* { dg-warning "length modifier" } */
  printf ("%w32A", i32); /* { dg-warning "length modifier" } */
  printf ("%w64A", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8A", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16A", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32A", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64A", if64); /* { dg-warning "length modifier" } */
  printf ("%w8c", i8); /* { dg-warning "length modifier" } */
  printf ("%w16c", i16); /* { dg-warning "length modifier" } */
  printf ("%w32c", i32); /* { dg-warning "length modifier" } */
  printf ("%w64c", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8c", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16c", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32c", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64c", if64); /* { dg-warning "length modifier" } */
  printf ("%w8e", i8); /* { dg-warning "length modifier" } */
  printf ("%w16e", i16); /* { dg-warning "length modifier" } */
  printf ("%w32e", i32); /* { dg-warning "length modifier" } */
  printf ("%w64e", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8e", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16e", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32e", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64e", if64); /* { dg-warning "length modifier" } */
  printf ("%w8E", i8); /* { dg-warning "length modifier" } */
  printf ("%w16E", i16); /* { dg-warning "length modifier" } */
  printf ("%w32E", i32); /* { dg-warning "length modifier" } */
  printf ("%w64E", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8E", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16E", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32E", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64E", if64); /* { dg-warning "length modifier" } */
  printf ("%w8f", i8); /* { dg-warning "length modifier" } */
  printf ("%w16f", i16); /* { dg-warning "length modifier" } */
  printf ("%w32f", i32); /* { dg-warning "length modifier" } */
  printf ("%w64f", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8f", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16f", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32f", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64f", if64); /* { dg-warning "length modifier" } */
  printf ("%w8F", i8); /* { dg-warning "length modifier" } */
  printf ("%w16F", i16); /* { dg-warning "length modifier" } */
  printf ("%w32F", i32); /* { dg-warning "length modifier" } */
  printf ("%w64F", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8F", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16F", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32F", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64F", if64); /* { dg-warning "length modifier" } */
  printf ("%w8g", i8); /* { dg-warning "length modifier" } */
  printf ("%w16g", i16); /* { dg-warning "length modifier" } */
  printf ("%w32g", i32); /* { dg-warning "length modifier" } */
  printf ("%w64g", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8g", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16g", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32g", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64g", if64); /* { dg-warning "length modifier" } */
  printf ("%w8G", i8); /* { dg-warning "length modifier" } */
  printf ("%w16G", i16); /* { dg-warning "length modifier" } */
  printf ("%w32G", i32); /* { dg-warning "length modifier" } */
  printf ("%w64G", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8G", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16G", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32G", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64G", if64); /* { dg-warning "length modifier" } */
  printf ("%w8p", i8); /* { dg-warning "length modifier" } */
  printf ("%w16p", i16); /* { dg-warning "length modifier" } */
  printf ("%w32p", i32); /* { dg-warning "length modifier" } */
  printf ("%w64p", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8p", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16p", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32p", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64p", if64); /* { dg-warning "length modifier" } */
  printf ("%w8s", i8); /* { dg-warning "length modifier" } */
  printf ("%w16s", i16); /* { dg-warning "length modifier" } */
  printf ("%w32s", i32); /* { dg-warning "length modifier" } */
  printf ("%w64s", i64); /* { dg-warning "length modifier" } */
  printf ("%wf8s", if8); /* { dg-warning "length modifier" } */
  printf ("%wf16s", if16); /* { dg-warning "length modifier" } */
  printf ("%wf32s", if32); /* { dg-warning "length modifier" } */
  printf ("%wf64s", if64); /* { dg-warning "length modifier" } */
}
