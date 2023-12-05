/* { dg-options "" } */

#include <arm_neon.h>
#include <arm_sme.h>

uint8x16_t *neon;
svint64_t *sve;
int64_t *ptr;

inline void __attribute__((always_inline))
call_vadd ()
{
  neon[4] = vaddq_u8 (neon[5], neon[6]);
}

inline void __attribute__((always_inline))
call_vbsl ()
{
  neon[0] = vbslq_u8 (neon[1], neon[2], neon[3]);
}

inline void __attribute__((always_inline))
call_svadd ()
{
  *sve = svadd_x (svptrue_b8 (), *sve, 1);
}

inline void __attribute__((always_inline))
call_svld1_gather ()
{
  *sve = svld1_gather_offset (svptrue_b8 (), ptr, *sve);
}

inline void __attribute__((always_inline))
call_svzero () [[arm::inout("za")]]
{
  svzero_za ();
}

inline void __attribute__((always_inline))
call_svst1_za () [[arm::streaming, arm::inout("za")]] // { dg-error "inlining failed" }
{
  svst1_ver_za64 (0, 0, svptrue_b8 (), ptr);
}

void
n_caller () [[arm::inout("za")]]
{
  call_vadd ();
  call_vbsl ();
  call_svadd ();
  call_svld1_gather ();
  call_svzero ();
  call_svst1_za ();
}
