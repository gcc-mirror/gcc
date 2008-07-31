/* PR middle-end/3074  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */

void foo(int a)
{
  5 * (a == 1) | (a == 2);  /* { dg-warning "no effect" "no effect" } */
}

