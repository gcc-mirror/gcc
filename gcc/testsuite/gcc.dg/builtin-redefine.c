/* Test -Wno-builtin-macro-redefined warnings.  */

/* { dg-do compile } */
/* { dg-options "-Wno-builtin-macro-redefined -U__DATE__ -D__TIME__=X" } */

/* Check date, time, and datestamp built-ins warnings may be suppressed.  */

#if defined(__DATE__)
#error "__DATE__ is defined, but should not be (-U command line error)"
/* { dg-bogus "__DATE__ is defined" "" { target *-*-* } 9 } */
#endif

#if __TIME__ != X
#error "__TIME__ is not defined as expected (-D command line error)"
/* { dg-bogus "__TIME__ is not defined" "" { target *-*-* } 14 } */
#endif

#if !defined(__TIMESTAMP__)
#error "__TIMESTAMP__ is not defined (built-in macro expectation error)"
/* { dg-bogus "__TIMESTAMP__ is not defined" "" { target *-*-* } 19 } */
#endif


#undef __TIME__              /* Undefine while defined.  */
#undef __TIME__              /* Undefine while already undefined.  */

#define __TIME__ "X"         /* Define while undefined.  */
#define __TIME__ "X"         /* Re-define while defined.  */

#define __TIME__ "Y"         /* { dg-warning "\"__TIME__\" redefined" } */
/* { dg-warning "previous definition" "" { target *-*-* } 28 } */

#undef __TIME__              /* Undefine while defined.  */


#undef __DATE__              /* Undefine while already undefined.  */

#define __DATE__ "X"         /* Define while undefined.  */
#define __DATE__ "X"         /* Re-define while defined.  */

#define __DATE__ "Y"         /* { dg-warning "\"__DATE__\" redefined" } */
/* { dg-warning "previous definition" "" { target *-*-* } 39 } */

#undef __DATE__              /* Undefine while defined.  */


#define __TIMESTAMP__ "X"    /* Define while already defined.  */
#define __TIMESTAMP__ "X"    /* Re-define while defined.  */

#define __TIMESTAMP__ "Y"    /* { dg-warning "\"__TIMESTAMP__\" redefined" } */
/* { dg-warning "previous definition" "" { target *-*-* } 48 } */

#undef __TIMESTAMP__         /* Undefine while defined.  */


/* Check other built-ins with warnings that may be suppressed.  */

#if !defined(__FILE__) || !defined(__BASE_FILE__)
#error "Expected built-in is not defined (built-in macro expectation error)"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } 59 } */
#endif

#define __FILE__ "X"         /* Define while already defined.  */
#define __BASE_FILE__ "X"    /* Define while already defined.  */


/* Check selected built-ins not affected by warning suppression. */

#if !defined(__LINE__) || !defined(__INCLUDE_LEVEL__) || !defined(__COUNTER__)
#error "Expected built-in is not defined (built-in macro expectation error)"
/* { dg-bogus "Expected built-in is not defined" "" { target *-*-* } 70 } */
#endif

#define __LINE__ 0           /* { dg-warning "\"__LINE__\" redef" } */
#define __INCLUDE_LEVEL__ 0  /* { dg-warning "\"__INCLUDE_LEVEL__\" redef" } */
#define __COUNTER__ 0        /* { dg-warning "\"__COUNTER__\" redef" } */


int unused;  /* Silence `ISO C forbids an empty translation unit' warning.  */
