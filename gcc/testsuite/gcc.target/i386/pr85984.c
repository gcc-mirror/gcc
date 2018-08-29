/* PR target/85984 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int foo (void);

void __attribute__((naked))
bar (void)
{
  if (!foo ())
    __builtin_abort ();
}

void
baz (void)
{
  bar ();
}
