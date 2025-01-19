/* { dg-do compile } */

extern void foo (void);

void test_nesting (void)
{
  foo (); /* { dg-error "top-level error" } */
}
