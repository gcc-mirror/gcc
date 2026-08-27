/* { dg-do compile { target lp64 } } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <stdint.h>

/*
 * PR target/117487
 *
 * On power8 with the optimization it generates:
 *
 *	xscvdpspn 0,1
 *	sldi 9,4,32
 *	mtvsrd 32,9
 *	xxland 1,0,32
 *	xscvspdpn 1,1
 *
 * I.e., it converts the SFmode to the memory format (instead of the DFmode
 * that is used within the register), converts the mask so that it is in the
 * vector register in the upper 32-bits, and does a XXLAND (i.e. there is only
 * one direct move from GPR to vector register).  Then after doing this, it
 * converts the upper 32-bits back to DFmode.
 *
 * If the XSCVSPDN instruction took the value in the normal 32-bit scalar in a
 * vector register, we wouldn't have needed the SLDI of the mask.
 *
 * On power9/power10/power11 before the fix wa applied, GCC generated:
 *
 *	xscvdpspn 0,1
 *	mfvsrwz 2,0
 *	and 2,2,4
 *	mtvsrws 1,2
 *	xscvspdpn 1,1
 *	blr
 *
 * I.e convert to SFmode representation, move the value to a GPR, do an AND
 * operation, move the 32-bit value with a splat, and then convert it back to
 * DFmode format.
 *
 * After the patch was applied, it now generates:
 *
 *	xscvdpspn 0,1
 *	mtvsrwz 32,2
 *	xxland 32,0,32
 *	xxspltw 1,32,1
 *	xscvspdpn 1,1
 *	blr
 */

union u {
  float f;
  uint32_t u32;
};

float
math_foo (float x, unsigned int mask)
{
  union u arg;

  arg.f = x;
  arg.u32 &= mask;
  return arg.f;
}

/* { dg-final { scan-assembler     {\mxxland\M} } } */
/* { dg-final { scan-assembler-not {\mmfvsr}    } } */
/* { dg-final { scan-assembler-not {\mand\M}    } } */
