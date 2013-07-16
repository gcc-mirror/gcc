/* PR debug/57184 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

struct S {};
void bar (struct S *const);
static struct S *const c = &(struct S) {};

void
foo (void)
{
  bar (c);
}
