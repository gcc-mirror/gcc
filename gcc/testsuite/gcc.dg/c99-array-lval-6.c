/* PR c/12446 */
/* Origin: Keith Thompson <kst@cts.com> */

/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */


struct s { char c[1]; };

extern struct s foo(void);

void bar(void)
{
  char *ptr = foo().c; /* { dg-bogus "non-lvalue" "array not decaying to lvalue" } */
}
