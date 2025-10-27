/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -msve-vector-bits=256 -mlittle-endian" } */

#include <arm_sve.h>

#pragma GCC target "+sve"

svint64x2_t __RTL (startwith ("vregs")) foo ()
{
  (function "foo"
    (insn-chain
      (block 2
	(edge-from entry (flags "FALLTHRU"))
	(cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
        (cnote 2 NOTE_INSN_FUNCTION_BEG)
	(insn 3 (set (reg:VNx4DI <0>)
		     (const_vector:VNx4DI [(const_int 11)
					   (const_int 12)
					   (const_int 13)
					   (const_int 14)
					   (const_int 15)
					   (const_int 16)
					   (const_int 17)
					   (const_int 18)])))
	(insn 4 (set (reg:VNx4DI v0) (reg:VNx4DI <0>)))
        (insn 5 (use (reg:VNx4DI v0)))
	(edge-to exit (flags "FALLTHRU"))
      ) ;; block 2
    ) ;; insn-chain
    (crtl (return_rtx (reg:VNx4DI v0)))
  ) ;; function
}

/* { dg-final { scan-assembler {\tindex\tz0\.d, #11, #1\n} } } */
/* { dg-final { scan-assembler {\tindex\tz1\.d, #15, #1\n} } } */
