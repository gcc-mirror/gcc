/* Test C23 enumerations with values not representable in int.  Test overflow
   of __int128 is diagnosed.  */
/* { dg-do compile { target { int128 } } } */
/* { dg-options "-std=c23" } */

enum e1 { e1a = (__int128) (((unsigned __int128) -1) >> 1), e1b }; /* { dg-error "overflow in enumeration values" } */

enum e2 { e2a = (unsigned __int128) -1, e2b }; /* { dg-error "overflow in enumeration values" } */

/* Likewise, when it's the enum as a whole that can't fit in __int128 or
   unsigned __int128, but the individual enumerators fit (some fitting __int128
   and some fitting unsigned __int128).  */
enum e3 { e3a = -(__int128) (((unsigned __int128) -1) >> 1) - 1,
	  e3b = (unsigned __int128) -1 }; /* { dg-warning "enumeration values exceed range of largest integer" } */
