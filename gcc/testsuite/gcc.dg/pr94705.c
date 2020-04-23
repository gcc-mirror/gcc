/* PR c/94705 */
/* { dg-do compile } */
/* { dg-options "" } */

void foo ();

int
bar (void)
{
  foo (baz);	/* { dg-error "'baz' undeclared" } */
		/* { dg-message "only once" "" { target *-*-* } .-1 } */
  void __attribute__ ((noinline)) baz (void);
}
