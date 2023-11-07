/* Test for printf formats: rejection of C23 (and C23-recommended) formats in
   pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wformat" } */

#include "format.h"

void
foo (int i, int_least8_t i8, int_least16_t i16, int_least32_t i32,
     int_least64_t i64, uint_least8_t u8, uint_least16_t u16,
     uint_least32_t u32, uint_least64_t u64, int_fast8_t if8,
     int_fast16_t if16, int_fast32_t if32, int_fast64_t if64, uint_fast8_t uf8,
     uint_fast16_t uf16, uint_fast32_t uf32, uint_fast64_t uf64)
{
  printf ("%b", i); /* { dg-warning "C" } */
  printf ("%B", i); /* { dg-warning "C" } */
  printf ("%w8d", i8); /* { dg-warning "C" } */
  printf ("%w16d", i16); /* { dg-warning "C" } */
  printf ("%w32d", i32); /* { dg-warning "C" } */
  printf ("%w64d", i64); /* { dg-warning "C" } */
  printf ("%wf8d", if8); /* { dg-warning "C" } */
  printf ("%wf16d", if16); /* { dg-warning "C" } */
  printf ("%wf32d", if32); /* { dg-warning "C" } */
  printf ("%wf64d", if64); /* { dg-warning "C" } */
  printf ("%w8u", u8); /* { dg-warning "C" } */
  printf ("%w16u", u16); /* { dg-warning "C" } */
  printf ("%w32u", u32); /* { dg-warning "C" } */
  printf ("%w64u", u64); /* { dg-warning "C" } */
  printf ("%wf8u", uf8); /* { dg-warning "C" } */
  printf ("%wf16u", uf16); /* { dg-warning "C" } */
  printf ("%wf32u", uf32); /* { dg-warning "C" } */
  printf ("%wf64u", uf64); /* { dg-warning "C" } */
  printf ("%w8i", i8); /* { dg-warning "C" } */
  printf ("%w8o", u8); /* { dg-warning "C" } */
  printf ("%w8x", u8); /* { dg-warning "C" } */
  printf ("%w8X", u8); /* { dg-warning "C" } */
  printf ("%w8n", &i8); /* { dg-warning "C" } */
}
