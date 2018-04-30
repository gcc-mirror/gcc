/* PR middle-end/79499 */
/* { dg-do compile { target split_stack } } */
/* { dg-options "-O2 -fsplit-stack -fno-omit-frame-pointer" } */

struct S { struct S *a, *b; };

void
foo (struct S *x)
{
  do
    x->b = x->a;
  while (x = x->a);
}
