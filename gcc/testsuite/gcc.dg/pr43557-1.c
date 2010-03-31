/* PR debug/43557 */
/* { dg-do compile } */
/* { dg-options "--combine -g -O2" } */
/* { dg-additional-sources "pr43557-2.c" } */

struct S
{
  int v;
} g;

void
f1 (void)
{
  struct S *s = &g;
  s->v = 0;
}
