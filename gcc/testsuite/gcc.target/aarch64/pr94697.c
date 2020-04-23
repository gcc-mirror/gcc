/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=standard" } */
/* { dg-final { check-function-bodies "**" "" } } */

void bar (int *);
void *addr;

/*
** foo:
**	hint	(25|34|38) // (paciasp|bti c|bti jc)
**	...
*/
int foo (int x)
{
label:
  addr = &&label;
  bar (&x);
  return x;
}
