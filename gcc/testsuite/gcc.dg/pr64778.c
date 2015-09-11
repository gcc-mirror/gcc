/* PR c/64778 */
/* { dg-do compile } */

int
foo (int p)
{
  int a;
  a ^= foo (,);	/* { dg-error "expected expression before|too many arguments" } */
  return a;
}
