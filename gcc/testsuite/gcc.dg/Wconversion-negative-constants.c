/* Test for diagnostics for negative constants converted to unsigned types.
   These tests come from gcc/testsuite/gcc.dg/overflow-warn-2.c  */

/* { dg-do compile } */
/* { dg-options "-std=c99 -Wconversion" } */

#include <limits.h>

void fuc (unsigned char);

void hc (int x)
{
  unsigned char uc;

  fuc (-1); /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  uc = -1; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  uc = x ? 1U : -1; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  uc = x ? SCHAR_MIN : 1U; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  uc = '\xa0'; /* { dg-warning "negative integer implicitly converted to unsigned type" } */

  fuc('A');
  uc = 'A';

  uc = x ? 1 : -1;

  uc = x ? SCHAR_MIN : 1;
}

unsigned fui (unsigned int ui);

void hi (int x)
{
  unsigned ui;

  fui (-1); /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  ui = -1; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  ui = x ? 1U : -1; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  ui = x ? INT_MIN : 1U; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  ui = ui ? SCHAR_MIN : 1U; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  ui = 1U * -1; /* { dg-warning "negative integer implicitly converted to unsigned type" } */
  ui = ui + INT_MIN; /* { dg-warning "negative integer implicitly converted to unsigned type" } */

  ui = -1 * (1 * -1);
  ui = (unsigned) -1;

  ui = x ? 1 : -1;

  ui = x ? INT_MIN : 1;

  ui = ui ? SCHAR_MIN : 1;
}


unsigned fui(unsigned a) { return a + -1; } /* { dg-warning "negative integer implicitly converted to unsigned type" } */
