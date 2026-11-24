/* { dg-do assemble { target aarch64_asm_sve2p2_ok } } */
/* { dg-do compile { target { ! aarch64_asm_sve2p2_ok } } } */
/* { dg-options "-O2 -msve-vector-bits=256 -save-temps" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>

#pragma GCC target "+sve2p2"

#ifdef __cplusplus
extern "C" {
#endif

/*
** test1:
**	mov	w0, 31 
**	ret
*/
uint64_t
test1 ()
{
  return svlastp_b8 (svptrue_b8 (),
		     svptrue_b8 ());
}

/*
** test2:
**	mov	x0, -1 
**	ret
*/
uint64_t
test2 ()
{
  return svlastp_b8 (svpfalse_b (),
		     svptrue_b8 ());
}

/*
** test3:
**	mov	x0, -1
**	ret
*/
uint64_t
test3 ()
{
  return svlastp_b8 (svptrue_b8 (),
		     svpfalse_b ());
}

/*
** test4:
**	mov	w0, 31 
**	ret
*/
uint64_t
test4 ()
{
  return svlastp_b8 (svdupq_n_b8 (false, false, false, false,
				   false, false, false, false,
				   false, false, false, false,
				   false, false, false, true),
		     svptrue_b8 ());
}

/*
** test5:
**	mov	w0, 15 
**	ret
*/
uint64_t
test5 ()
{
  return svlastp_b16 (svptrue_b16 (),
		      svptrue_b16 ());
}

/*
** test6:
**	mov	x0, -1 
**	ret
*/
uint64_t
test6 ()
{
  return svlastp_b16 (svpfalse_b (),
		      svptrue_b16 ());
}

/*
** test7:
**	mov	x0, -1
**	ret
*/
uint64_t
test7 ()
{
  return svlastp_b16 (svptrue_b16 (),
		      svpfalse_b ());
}

/*
** test8:
**	mov	w0, 15 
**	ret
*/
uint64_t
test8 ()
{
  return svlastp_b16 (svdupq_n_b16 (false, false, false, false,
				    false, false, false, true),
		      svptrue_b16 ());
}

/*
** test9:
**	mov	w0, 7 
**	ret
*/
uint64_t
test9 ()
{
  return svlastp_b32 (svptrue_b32 (),
		      svptrue_b32 ());
}

/*
** test10:
**	mov	x0, -1 
**	ret
*/
uint64_t
test10 ()
{
  return svlastp_b32 (svpfalse_b (),
		      svptrue_b32 ());
}

/*
** test11:
**	mov	x0, -1
**	ret
*/
uint64_t
test11 ()
{
  return svlastp_b32 (svptrue_b32 (),
		      svpfalse_b ());
}

/*
** test12:
**	mov	w0, 7 
**	ret
*/
uint64_t
test12 ()
{
  return svlastp_b32 (svdupq_n_b32 (false, false, false, true),
		      svptrue_b32 ());
}

/*
** test13:
**	mov	w0, 3 
**	ret
*/
uint64_t
test13 ()
{
  return svlastp_b64 (svptrue_b64 (),
		      svptrue_b64 ());
}

/*
** test14:
**	mov	x0, -1 
**	ret
*/
uint64_t
test14 ()
{
  return svlastp_b64 (svpfalse_b (),
		      svptrue_b64 ());
}

/*
** test15:
**	mov	x0, -1
**	ret
*/
uint64_t
test15 ()
{
  return svlastp_b64 (svptrue_b64 (),
		      svpfalse_b ());
}

/*
** test16:
**	mov	w0, 3
**	ret
*/
uint64_t
test16 ()
{
  return svlastp_b64 (svdupq_n_b64 (false, true),
		      svptrue_b64 ());
}

#ifdef __cplusplus
}
#endif
