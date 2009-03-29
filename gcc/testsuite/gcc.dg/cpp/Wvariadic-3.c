/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Werror" } */

#define f(x,...)
#define g(x,y...)	/* { dg-error "variadic" } */
int not_empty;
/* { dg-message "warnings being treated as errors" "" { target *-*-* } 0 } */
