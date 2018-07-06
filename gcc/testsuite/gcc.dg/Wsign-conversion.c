/* Test for diagnostics for implicit conversions between signed and
   unsigned integer types.
   These tests come from gcc/testsuite/gcc.dg/overflow-warn-2.c  */

/* { dg-do compile } */
/* { dg-options "-std=c99 -fsigned-char -Wsign-conversion" } */
#include <limits.h>

void fsc (signed char sc);
void fuc (unsigned char uc);
unsigned fui (unsigned int  ui);
void fsi (signed int ui);
void fsi32 (__INT32_TYPE__ ui);

void h (int x)
{
  unsigned int ui = 3;
  int   si = 3;
  __INT32_TYPE__ si32 = 3;
  unsigned char uc = 3;
  signed char   sc = 3;

  uc = ui; 
  uc = si; 
  sc = ui; 
  sc = si; 
  fuc (ui);
  fuc (si);
  fsc (ui);
  fsc (si);

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

  uc = x ? 1U : -1; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value from .-1. to .\[0-9\]+." } */
  uc = x ? SCHAR_MIN : 1U;  /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value from .-\[0-9\]+. to .\[0-9\]+." } */
  uc = x ? 1 : -1; /* { dg-warning "unsigned conversion from .int. to .unsigned char. changes the value of .-1." } */
  uc = x ? SCHAR_MIN : 1; /* { dg-warning "unsigned conversion from .int. to .unsigned char. changes the value of .-\[0-9\]+." } */
  ui = x ? 1U : -1; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value from .-1. to .\[0-9\]+." } */
  ui = x ? INT_MIN : 1U; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value from .-\[0-9\]+. to .\[0-9\]+." } */
  ui = ui ? SCHAR_MIN : 1U; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value " } */
  ui = 1U * -1; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value " } */
  ui = ui + INT_MIN; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value " } */
  ui = x ? 1 : -1; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes the value of .-1." } */
  ui = ui ? SCHAR_MIN : 1; /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes the value of " } */

  fuc (-1); /* { dg-warning "unsigned conversion" } */
  uc = -1;  /* { dg-warning "unsigned conversion" } */
  fui (-1); /* { dg-warning "unsigned conversion" } */
  ui = -1; /* { dg-warning "unsigned conversion" } */
  fuc ('\xa0'); /* { dg-warning "unsigned conversion" } */
  uc = '\xa0'; /* { dg-warning "unsigned conversion" } */
  fui ('\xa0');/* { dg-warning "unsigned conversion" } */
  ui = '\xa0'; /* { dg-warning "unsigned conversion" } */
  fsi32 (0x80000000); /* { dg-warning "signed conversion" } */
  si32 = 0x80000000;  /* { dg-warning "signed conversion" } */


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

unsigned fui (unsigned a) { return a + -1; } /* { dg-warning "unsigned conversion from .int. to .unsigned int. changes value" } */


