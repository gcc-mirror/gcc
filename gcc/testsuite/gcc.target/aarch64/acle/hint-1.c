/* Test the ACLE hint intrinsics.  */
/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_acle.h>

/*
** test_hint:
** ...
**	yield
**	wfe
**	sev
**	sevl
**	wfi
**...
**	ret
*/
void
test_hint ()
{
  __yield ();
  __wfe ();
  __sev ();
  __sevl ();
  __wfi ();
}
