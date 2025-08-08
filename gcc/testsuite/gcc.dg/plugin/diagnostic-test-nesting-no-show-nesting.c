/* { dg-do compile } */
/* { dg-options "-fno-diagnostics-show-nesting" } */

extern void foo (void);

void test_nesting (void)
{
  foo (); /* { dg-error "top-level error" } */
}
