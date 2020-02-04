/* { dg-do preprocess } */

#if __has_include_next ("stdlib.h")
#else
#error error 1
#endif
#if __has_include_next (<stdlib.h>)
#else
#error error 2
#endif
#if !__has_include_next ("stdlib.h")
#error error 3
#elif !__has_include_next (<stdlib.h>)
#error error 4
#endif
#if __has_include_next ("stdlib.h") && __has_include_next (<stdlib.h>)
#else
#error error 5
#endif
#if !defined(__has_include_next)
#error error 6
#endif
#ifndef __has_include_next
#error error 7
#endif
#ifdef __has_include_next
#else
#error error 8
#endif
#define m1 __has_include_next("stdlib.h")
#define m2 ("stdlib.h")
#define m3 ("has-include-1-nonexistent.h")
#define m4 has-include-1-nonexistent-2.h>)
#define m5 <stdlib.h>
#if !m1
#error error 9
#endif
#if !__has_include_next m2
#error error 10
#endif
#if __has_include_next m3
#error error 11
#endif
#if __has_include_next (<m4
#error error 12
#endif
#if !__has_include_next (m5)
#error error 13
#endif
__has_include_next (<stdlib.h>)		/* { dg-error "used outside of preprocessing directive" } */
m1					/* { dg-error "used outside of preprocessing directive" } */
#if 1
m1					/* { dg-error "used outside of preprocessing directive" } */
#endif
#if 0
#elif 1
m1					/* { dg-error "used outside of preprocessing directive" } */
#endif
#if 0
m1
#endif
#if 0
#elif 0
m1
#endif
#if __has_include_next "stdlib.h")	/* { dg-error "missing" } */
#endif
#if __has_include_next (stdlib.h)	/* { dg-error "operator|missing" } */
#endif
#if __has_include_next ()		/* { dg-error "operator|missing" } */
#endif
#if __has_include_next )		/* { dg-error "operator|missing" } */
#endif
#if __has_include_next ("stdlib.h)
#endif
/* { dg-error "operator|missing\[^\n\r]*after" "" { target *-*-* } .-2 } */
/* { dg-warning "missing terminating" "" { target *-*-* } .-3 } */
#if __has_include_next (stdlib.h>)	/* { dg-error "operator|missing" } */
#endif
#if __has_include_next ("stdlib.h"	/* { dg-error "missing" } */
#endif
#if __has_include_next (		/* { dg-error "operator|missing" } */
#endif
#if __has_include_next			/* { dg-error "operator|missing" } */
#endif
#if __has_include_next"stdlib.h"	/* { dg-error "missing" } */
#endif
#if __has_include_next'h'		/* { dg-error "operator|missing" } */
#endif
#if __has_include_next('h'		/* { dg-error "operator|missing" } */
#endif
#if __has_include_next('h')		/* { dg-error "operator" } */
#endif
#define H(h) __has_include_next(h)
#if H(<stdlib.h>)
#else
#error error 14
#endif
void
foo ()
{
#pragma omp parallel if (__has_include_next ("<stdlib.h>"))
  ;
}
