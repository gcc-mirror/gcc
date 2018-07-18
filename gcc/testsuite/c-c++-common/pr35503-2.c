/* { dg-do compile } */
/* { dg-options "-fdiagnostics-show-caret -Wrestrict" } */

void f(int *__restrict x, int *y, int *__restrict z, int *w);

void foo(int alpha, int beta)
{
  f (&alpha, &beta, &alpha, &alpha); /* { dg-warning "passing argument 1 to restrict-qualified parameter aliases with arguments 3, 4" } */

/* { dg-begin-multiline-output "" }
   f (&alpha, &beta, &alpha, &alpha);
      ^~~~~~         ~~~~~~  ~~~~~~
   { dg-end-multiline-output "" } */
}
