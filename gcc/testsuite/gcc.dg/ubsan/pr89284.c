/* PR middle-end/89284 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -O0 -Wuninitialized" } */

struct A { _Bool a; int i; };

int
foo (void)
{
  struct A a;
  if (a.i)	/* { dg-warning "'a.i' is used uninitialized" } */
    return 1;
  return 0;
}

int
bar (void)
{
  struct A a;
  if (a.a)	/* { dg-warning "'a.a' is used uninitialized" } */
    return 1;
  return 0;
}
