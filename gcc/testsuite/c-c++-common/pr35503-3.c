/* { dg-do compile } */
/* { dg-options "-Wrestrict" } */

void f(int *x, int *__restrict y);

void foo(int a)
{
  f (&a, &a); /* { dg-warning "passing argument 2 to restrict-qualified parameter aliases with argument 1" } */
}
