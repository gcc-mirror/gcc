/* { dg-do compile } */
/* { dg-options "-mcmse -fshort-enums" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_cmse.h>
#include <stdbool.h>

enum offset
{
    zero = 0,
    one = 1,
    two = 2
};

typedef unsigned char __attribute__ ((cmse_nonsecure_call)) ns_unsign_foo_t (void);
typedef signed char __attribute__ ((cmse_nonsecure_call)) ns_sign_foo_t (void);
typedef unsigned short __attribute__ ((cmse_nonsecure_call)) ns_short_unsign_foo_t (void);
typedef signed short __attribute__ ((cmse_nonsecure_call)) ns_short_sign_foo_t (void);
typedef enum offset __attribute__ ((cmse_nonsecure_call)) ns_enum_foo_t (void);
typedef bool __attribute__ ((cmse_nonsecure_call)) ns_bool_foo_t (void);

/*
**unsignNonsecure0:
**	...
**	bl	__gnu_cmse_nonsecure_call
**	uxtb	r0, r0
**	...
*/
unsigned char unsignNonsecure0 (ns_unsign_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}

/*
**signNonsecure0:
**	...
**	bl	__gnu_cmse_nonsecure_call
**	sxtb	r0, r0
**	...
*/
signed char signNonsecure0 (ns_sign_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}

/*
**shortUnsignNonsecure0:
**	...
**	bl	__gnu_cmse_nonsecure_call
**	uxth	r0, r0
**	...
*/
unsigned short shortUnsignNonsecure0 (ns_short_unsign_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}

/*
**shortSignNonsecure0:
**	...
**	bl	__gnu_cmse_nonsecure_call
**	sxth	r0, r0
**	...
*/
signed short shortSignNonsecure0 (ns_short_sign_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}

/*
**enumNonsecure0:
**	...
**	bl	__gnu_cmse_nonsecure_call
**	uxtb	r0, r0
**	...
*/
unsigned char __attribute__((noipa)) enumNonsecure0 (ns_enum_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}

/*
**boolNonsecure0:
**	...
**	bl	__gnu_cmse_nonsecure_call
**	uxtb	r0, r0
**	...
*/
unsigned char boolNonsecure0 (ns_bool_foo_t * ns_foo_p)
{
  return ns_foo_p ();
}
