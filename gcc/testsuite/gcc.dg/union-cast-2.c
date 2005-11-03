/* { dg-do compile } */
/* { dg-options "-std=c89 -pedantic-errors" } */
/* PR 23155
   We should get two error messages, one about union cast
   and the other about array access for non lvalues.  */


union vx {short f[8]; int v;};
int vec;

void
foo5 (int vec)
{
  ((union vx) vec).f[5] = 1; /* { dg-error "(forbids subscripting)|(forbids casts to union type)" } */
}
