/* PR debug/29609 */
/* Verify that breakpoint on both goto failure; stmts is hit.  */
/* { dg-do compile } */
/* { dg-options "-O0 -gdwarf -dA" } */

extern void abort (void);
int x;

int
foo (void)
{
  return 0 ^ x;
}

int
bar (void)
{
  return 1 ^ x;
}

int
baz (void)
{
  int c;

  if (!foo ())
    goto failure;

  if (!bar ())
    goto failure;

  return 0;

failure:
  return 1;
}

int
main (void)
{
  if (baz () != 1)
    abort ();
  x = 1;
  if (baz () != 1)
    abort ();
  x = 2;
  if (baz () != 0)
    abort ();
  return 0;
}

/* { dg-final { scan-assembler "pr29609-2.c:27" } } */
/* { dg-final { scan-assembler "pr29609-2.c:30" } } */
