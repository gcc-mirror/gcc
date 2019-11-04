/* PR tree-optimization/92226 - live nul char store to array eliminated
   Test to verify warnings are issued for overflow detected thanks to
   the enhancement committed on top of the fix for PR 92226.
   { dg-do compile }
   { dg-options "-O2 -Wall -Wno-array-bounds -ftrack-macro-expansion=0" } */

#include "strlenopt.h"

#define NOIPA __attribute__ ((noipa))

#define T(MIN, MAX, SIZE, IDX)						\
  NOIPA void								\
  test_ ## MIN ## _ ## MAX ## _ ## SIZE ## _ ## IDX (const char *s)	\
  {									\
    size_t len = strlen (s);						\
    if (MIN <= len && len <= MAX)					\
      {									\
	extern char d[];						\
	strcpy (d, s);							\
	d[IDX] = 0;							\
	extern char a ## SIZE[SIZE];					\
	strcpy (a ## SIZE, d);						\
      }									\
  } typedef void dummy_type


T (2, 2, 1, 0);
T (2, 2, 1, 1);     // { dg-warning "writing 2 bytes into a region of size 1" }
T (2, 2, 1, 2);     // { dg-warning "writing 3 bytes into a region of size 1" }

T (2, 3, 1, 0);
T (2, 3, 1, 1);     // { dg-warning "writing 2 bytes into a region of size 1" }
T (2, 3, 1, 2);     // { dg-warning "writing 3 bytes into a region of size 1" "" { xfail *-*-*} }
T (2, 3, 1, 3);     // { dg-warning "writing 4 bytes into a region of size 1" "" { xfail *-*-* } }

T (5, 7, 3, 1);
T (5, 7, 3, 2);
T (5, 7, 3, 3);     // { dg-warning "writing 4 bytes into a region of size 3" }
T (5, 7, 3, 4);     // { dg-warning "writing 5 bytes into a region of size 3" }
T (5, 7, 3, 5);     // { dg-warning "writing 6 bytes into a region of size 3" "" { xfail *-*-* } }
