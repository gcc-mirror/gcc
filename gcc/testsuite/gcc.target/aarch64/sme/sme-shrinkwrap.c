/* { dg-options "-O3 -fshrink-wrap" } */
/* { dg-do run { target { aarch64_sme_hw && aarch64_sve_hw } } } */
/* { dg-do compile { target { ! { aarch64_sme_hw && aarch64_sve_hw } } } } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sme.h>

#pragma GCC target "+sve"

[[gnu::noipa]]
__arm_streaming
int callee (int x)
{
  return 0;
}

/*
** foo:
**	cbnz	w0, [^\n]*
**	cntd	x0
**	ret
**	...
*/
__arm_streaming
int foo(int x)
{
    if (x)
        return callee(3);
    return svcntd();
}

/*
** bar:
**	...
**	smstart	[^\n]*
**	...
** (
**	cntd	[^\n]*
**	...
**	cbn?z	[^\n]*
** |
**	cbn?z	[^\n]*
**	...
**	cntd	[^\n]*
** )
**	...
*/

__arm_locally_streaming
int bar(int x)
{
    if (x)
        return callee(3);
    return svcntd();
}

/*
** baz:
**	cbnz	w0, [^\n]*
**	cntd	x0
**	ret
**	...
*/
__arm_streaming
int baz(int x)
{
    if (x)
        return callee(3);
    return svcntd();
}

[[gnu::noipa]]
int main()
{
  if (bar(0) != svcntsd())
    __builtin_abort();
  return 0;
}
