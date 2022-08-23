/* PR middle-end/104558 */
/* { dg-do compile } */
/* { dg-options "-fabi-version=9" } */

struct __attribute__ ((aligned)) A {};

struct A a;

void bar (int, int, int, int, int, int, int, struct A);

void
foo (void)
{
  bar (0, 1, 2, 3, 4, 5, 6, a);
}
