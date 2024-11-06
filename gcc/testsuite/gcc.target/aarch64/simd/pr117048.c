/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

#pragma GCC target "+sha3"

/*
** func_shl_eor:
**	xar	v0\.2d, v([0-9]+)\.2d, v([0-9]+)\.2d, 63
**	ret 
*/
uint64x2_t
func_shl_eor (uint64x2_t a, uint64x2_t b) {
  uint64x2_t c = veorq_u64 (a, b);
  return veorq_u64(vshlq_n_u64(c, 1), vshrq_n_u64(c, 63));
}

/*
** func_add_eor:
**	xar	v0\.2d, v([0-9]+)\.2d, v([0-9]+)\.2d, 63
**	ret 
*/
uint64x2_t
func_add_eor (uint64x2_t a, uint64x2_t b) {
  uint64x2_t c = veorq_u64 (a, b);
  return veorq_u64(vaddq_u64(c, c), vshrq_n_u64(c, 63));
}

/*
** func_shl_orr:
**	xar	v0\.2d, v([0-9]+)\.2d, v([0-9]+)\.2d, 63
**	ret 
*/
uint64x2_t
func_shl_orr (uint64x2_t a, uint64x2_t b) {
  uint64x2_t c = veorq_u64 (a, b);
  return vorrq_u64(vshlq_n_u64(c, 1), vshrq_n_u64(c, 63));
}

/*
** func_add_orr:
**	xar	v0\.2d, v([0-9]+)\.2d, v([0-9]+)\.2d, 63
**	ret 
*/
uint64x2_t
func_add_orr (uint64x2_t a, uint64x2_t b) {
  uint64x2_t c = veorq_u64 (a, b);
  return vorrq_u64(vaddq_u64(c, c), vshrq_n_u64(c, 63));
}

/*
** func_shl_add:
**	xar	v0\.2d, v([0-9]+)\.2d, v([0-9]+)\.2d, 63
**	ret 
*/
uint64x2_t
func_shl_add (uint64x2_t a, uint64x2_t b) {
  uint64x2_t c = veorq_u64 (a, b);
  return vaddq_u64(vshlq_n_u64(c, 1), vshrq_n_u64(c, 63));
}

/*
** func_add_add:
**	xar	v0\.2d, v([0-9]+)\.2d, v([0-9]+)\.2d, 63
**	ret 
*/
uint64x2_t
func_add_add (uint64x2_t a, uint64x2_t b) {
  uint64x2_t c = veorq_u64 (a, b);
  return vaddq_u64(vaddq_u64(c, c), vshrq_n_u64(c, 63));
}
