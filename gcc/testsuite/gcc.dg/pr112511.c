/* PR middle-end/112511 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-O2" } */

struct T { _BitInt(22) a; };

void
bar (struct T t)
{
}

void
foo (void)
{
  struct T t;
  bar (t);
}
