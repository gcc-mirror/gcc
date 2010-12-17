/* PR debug/43557 */
/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -g -O2" } */
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

int main() { return 0; }
