/* Test for printf formats: rejection of C23 formats in pedantic mode.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic -Wformat" } */

#include "format.h"

void
foo (unsigned int *uip, int_least8_t *i8, int_least16_t *i16,
     int_least32_t *i32, int_least64_t *i64, uint_least8_t *u8,
     uint_least16_t *u16, uint_least32_t *u32, uint_least64_t *u64,
     int_fast8_t *if8, int_fast16_t *if16, int_fast32_t *if32,
     int_fast64_t *if64, uint_fast8_t *uf8, uint_fast16_t *uf16,
     uint_fast32_t *uf32, uint_fast64_t *uf64)
{
  scanf ("%b", uip); /* { dg-warning "C" } */
  scanf ("%w8d", i8); /* { dg-warning "C" } */
  scanf ("%w16d", i16); /* { dg-warning "C" } */
  scanf ("%w32d", i32); /* { dg-warning "C" } */
  scanf ("%w64d", i64); /* { dg-warning "C" } */
  scanf ("%wf8d", if8); /* { dg-warning "C" } */
  scanf ("%wf16d", if16); /* { dg-warning "C" } */
  scanf ("%wf32d", if32); /* { dg-warning "C" } */
  scanf ("%wf64d", if64); /* { dg-warning "C" } */
  scanf ("%w8u", u8); /* { dg-warning "C" } */
  scanf ("%w16u", u16); /* { dg-warning "C" } */
  scanf ("%w32u", u32); /* { dg-warning "C" } */
  scanf ("%w64u", u64); /* { dg-warning "C" } */
  scanf ("%wf8u", uf8); /* { dg-warning "C" } */
  scanf ("%wf16u", uf16); /* { dg-warning "C" } */
  scanf ("%wf32u", uf32); /* { dg-warning "C" } */
  scanf ("%wf64u", uf64); /* { dg-warning "C" } */
  scanf ("%w8i", i8); /* { dg-warning "C" } */
  scanf ("%w8o", u8); /* { dg-warning "C" } */
  scanf ("%w8x", u8); /* { dg-warning "C" } */
  scanf ("%w8X", u8); /* { dg-warning "C" } */
  scanf ("%w8n", i8); /* { dg-warning "C" } */
}
