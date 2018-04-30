/* Test for diagnostics for implicit conversions between integer types
   These tests come from gcc/testsuite/gcc.dg/overflow-warn-2.c  */

/* { dg-do compile } */
/* { dg-options "-std=c99 -fsigned-char -Wconversion" } */

#include <limits.h>

void fsc (signed char sc);
void fuc (unsigned char uc);
unsigned fui (unsigned int  ui);
void fsi (signed int ui);

void h (int x)
{
  unsigned int ui = 3;
  int   si = 3;
  unsigned char uc = 3;
  signed char   sc = 3;

  uc = ui; /* { dg-warning "conversion" } */
  uc = si; /* { dg-warning "conversion" } */
  sc = ui; /* { dg-warning "conversion" } */
  sc = si; /* { dg-warning "conversion" } */
  fuc (ui); /* { dg-warning "conversion" } */
  fuc (si); /* { dg-warning "conversion" } */
  fsc (ui); /* { dg-warning "conversion" } */
  fsc (si); /* { dg-warning "conversion" } */

  fsi (si);
  fui (ui);
  fsi (uc);
  si = uc;
  fui (uc);
  ui = uc;
  fui ('A');
  ui = 'A';
  fsi ('A');
  si = 'A';
  fuc ('A');
  uc = 'A';

  uc = x ? 1U : -1; /* { dg-warning "conversion from .unsigned int. to .unsigned char." } */
  /* { dg-warning "unsigned conversion" "implicit" { target *-*-* } .-1 } */
  uc = x ? SCHAR_MIN : 1U; /* { dg-warning "unsigned conversion" } */
  /* { dg-warning "conversion from .unsigned int. to .unsigned char." "implicit" { target *-*-* } .-1 } */
  uc = x ? 1 : -1; /* { dg-warning "signed conversion" } */
  uc = x ? SCHAR_MIN : 1; /* { dg-warning "signed conversion" } */
  ui = x ? 1U : -1; /* { dg-warning "signed conversion" } */
  ui = x ? INT_MIN : 1U; /* { dg-warning "signed conversion" } */
  ui = ui ? SCHAR_MIN : 1U; /* { dg-warning "signed conversion" } */
  ui = 1U * -1; /* { dg-warning "signed conversion" } */
  ui = ui + INT_MIN; /* { dg-warning "signed conversion" } */
  ui = x ? 1 : -1; /* { dg-warning "signed conversion" } */
  ui = ui ? SCHAR_MIN : 1; /* { dg-warning "signed conversion" } */

  fuc (-1); /* { dg-warning "signed conversion" } */
  uc = -1;  /* { dg-warning "signed conversion" } */
  fui (-1); /* { dg-warning "signed conversion" } */
  ui = -1; /* { dg-warning "signed conversion" } */
  fuc ('\xa0'); /* { dg-warning "signed conversion" } */
  uc = '\xa0'; /* { dg-warning "signed conversion" } */
  fui ('\xa0');/* { dg-warning "signed conversion" } */
  ui = '\xa0'; /* { dg-warning "signed conversion" } */
  fsi (0x80000000); /* { dg-warning "conversion" } */
  si = 0x80000000;  /* { dg-warning "conversion" } */


  fsi (UINT_MAX - 1);  /* { dg-warning "conversion" } */
  si = UINT_MAX - 1;   /* { dg-warning "conversion" } */
  fsi (UINT_MAX - 1U); /* { dg-warning "conversion" } */
  si = UINT_MAX - 1U;  /* { dg-warning "conversion" } */
  fsi (UINT_MAX/3U);
  si = UINT_MAX/3U;
  fsi (UINT_MAX/3);
  si = UINT_MAX/3;
  fui (UINT_MAX - 1);
  ui = UINT_MAX - 1;

  uc = (unsigned char) -1;
  ui = -1 * (1 * -1);
  ui = (unsigned) -1;

  fsc (uc); /* { dg-warning "conversion" } */
  sc = uc;  /* { dg-warning "conversion" } */
  fuc (sc); /* { dg-warning "conversion" } */
  uc = sc;  /* { dg-warning "conversion" } */
  fsi (ui); /* { dg-warning "conversion" } */
  si = ui;  /* { dg-warning "conversion" } */
  fui (si); /* { dg-warning "conversion" } */ 
  ui = si;  /* { dg-warning "conversion" } */ 
  fui (sc); /* { dg-warning "conversion" } */
  ui = sc;  /* { dg-warning "conversion" } */
}

unsigned fui (unsigned a) { return a + -1; } /* { dg-warning "signed conversion" } */


