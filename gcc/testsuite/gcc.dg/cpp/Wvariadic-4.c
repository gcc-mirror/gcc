/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Werror -Wno-variadic-macros" } */

#define f(x,...)
#define g(x,y...)
int not_empty;
