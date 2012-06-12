/* PR c/53196 */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

extern int printf (const char *, ...);
struct foo { int i; };

int
main ()
{
  struct foo f = (struct foo_typo) { };	/* { dg-error "invalid use of undefined type" } */
  printf ("%d\n", f.i);
  return 0;
}
