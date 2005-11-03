/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic-errors" } */
/* PR 23155
   We should get one error messag, one about union cast. */


union vx {short f[8]; int v;};
int vec;

void
foo5 (int vec)
{
  ((union vx) vec).f[5] = 1; /* { dg-error "forbids casts to union type" } */
}
