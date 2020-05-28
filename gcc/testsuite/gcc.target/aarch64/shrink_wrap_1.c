/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** foo:
**	...
**	str	d8, \[sp\]
**	ldr	d8, \[sp\]
**	...
*/
void
foo (int x)
{
  int tmp[0x1000];
  asm volatile ("" : "=m" (tmp));
  if (x == 1)
    asm volatile ("" ::: "d8");
}
