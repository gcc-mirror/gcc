/* PR c/94755 */
/* { dg-do compile } */

extern void foo (void);

void
bar (double x)
{
  if (x == __builtin_speculation_safe_value ())	/* { dg-error "too few arguments to function" } */
    foo ();
}
