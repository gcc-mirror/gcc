/* PR c/64223: Test for duplicated warnings.  */
/* { dg-do compile } */
/* { dg-options "-Wformat" } */

int printf (const char *, ...) __attribute__ ((__format__ (__printf__, 1, 2)));

void
foo (void)
{
  printf ("%d\n", 0UL); /* { dg-bogus "expects argument of type.*expects argument of type" } */
 /* { dg-warning "expects argument of type" "" { target *-*-* } .-1 } */
}
