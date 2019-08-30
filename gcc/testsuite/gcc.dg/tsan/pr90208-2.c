/* PR tree-optimization/90208 */
/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions -fsanitize=thread" } */

void *b[5];
void foo (void);

void
bar (int d)
{
  while (d)
    foo ();
}

void
baz (void)
{
  bar (2);
  __builtin_setjmp (b);
}
