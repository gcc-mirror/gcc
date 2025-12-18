/* { dg-do compile } */
/* { dg-options "-O2 -march=la32v1.0" } */
/* { dg-final { check-function-bodies "**" "" } } */

/*
** orn_la32r:
** 	nor	\$r5,\$r0,\$r5
** 	or	\$r4,\$r5,\$r4
** 	jr	\$r1
*/
#pragma GCC push_options
#pragma GCC target "arch=la32rv1.0"
int
orn_la32r (int a, int b)
{
  return a | ~b;
}

/*
** orn_la32s_attr:
** 	orn	\$r4,\$r4,\$r5
** 	jr	\$r1
*/
int
__attribute__ ((target ("arch=la32v1.0")))
orn_la32s_attr (int a, int b)
{
  return a | ~b;
}
#pragma GCC pop_options

/*
** orn_la32s:
** 	orn	\$r4,\$r4,\$r5
** 	jr	\$r1
*/
int
orn_la32s (int a, int b)
{
  return a | ~b;
}

/*
** orn_la32r_attr:
** 	nor	\$r5,\$r0,\$r5
** 	or	\$r4,\$r5,\$r4
** 	jr	\$r1
*/
int
__attribute__ ((target ("arch=la32rv1.0")))
orn_la32r_attr (int a, int b)
{
  return a | ~b;
}
