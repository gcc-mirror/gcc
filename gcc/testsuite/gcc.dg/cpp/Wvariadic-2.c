/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Werror -Wno-variadic-macros" } */

#define f(x,...)	/* { dg-bogus "variadic" } */
#define g(x,y...)	/* { dg-bogus "variadic" } */
int not_empty;
