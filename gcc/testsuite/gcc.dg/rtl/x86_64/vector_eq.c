/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-additional-options "-O2 -march=x86-64-v3" } */

typedef int v4si __attribute__((vector_size(16)));

v4si __RTL (startwith ("vregs")) foo (void)
{
(function "foo"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:V4SI <0>) (const_vector:V4SI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)])))
      (cinsn 5 (set (reg:V4SI <2>)
		    (eq:V4SI (reg:V4SI <0>) (reg:V4SI <1>))))
      (cinsn 6 (set (reg:V4SI <3>) (reg:V4SI <2>)))
      (cinsn 7 (set (reg:V4SI xmm0) (reg:V4SI <3>)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
 (crtl (return_rtx (reg/i:V4SI xmm0)))
)
}

/* { dg-final { scan-assembler-not "vpxor" } } */
