/* Register liveness information from epilgoue enables peephole optimization. */
/* { dg-do compile } */
/* { dg-options "-mthumb -Os" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { check-function-bodies "**" "" } } */

volatile int g_k;
extern void bar(int, int, int, int);

/*
** foo:
** ...
** (

Below block is for non-armv8.1-m.main
**	lsls	r[0-9]+, r[0-9]+, #29
**	it	mi
**	addmi	r2, r2, #1

** |

Below block is for armv8.1-m.main
**	tst	r[0-9]+, #4
**	csinc	r2, r2, r2, eq

** )
**	bl	bar
** ...
*/
int foo(int a, int b, int c, int d)
{
  if (g_k & 4) c++;
  bar (a, b, c, d);
  return 0;
}
