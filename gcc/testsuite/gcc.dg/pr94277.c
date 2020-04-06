/* PR debug/94277 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug" } */

static void foo (void);	/* { dg-warning "used but never defined" } */

void
bar (void)
{
  foo ();
}
