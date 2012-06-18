/* PR c/53196 */
/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */

extern int printf (const char *, ...);
struct foo { int i; };

int
main ()
{
  struct foo f = (struct foo_typo) { };	/* { dg-error "invalid use of undefined type" } */
  printf ("%d\n", f.i);
  return 0;
}

/* { dg-error "ISO C forbids empty initializer braces" "" { target *-*-* } 11 } */
