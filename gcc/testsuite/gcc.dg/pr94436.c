/* PR middle-end/94436 */
/* { dg-do compile } */
/* { dg-options "-Wincompatible-pointer-types" } */

struct S { int s; };
int foo (struct S *);

int
bar (void)
{
  int s = 0;
  return foo ((struct S *) ((char *) &s - (char *) &((struct S *) 0)->s));	/* { dg-bogus "from incompatible pointer type" } */
}
