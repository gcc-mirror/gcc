/* PR c/89061 */
/* { dg-do compile } */
/* { dg-options "-Wjump-misses-init" } */

struct S { int s; };

int
foo (int x)
{
  struct S s = { 0 };
  if ((s.s = x) == 0)
    goto cleanup;		/* { dg-bogus "jump skips variable initialization" } */
  s = (struct S) { .s = 42 };
 cleanup:
  return s.s;
}

int
bar (int x)
{
  struct S *s = &(struct S) { 0 };
  if ((s->s = x) == 0)
    goto cleanup;		/* { dg-bogus "jump skips variable initialization" } */
  s = &(struct S) { .s = 42 };
 cleanup:
  return s->s;
}
