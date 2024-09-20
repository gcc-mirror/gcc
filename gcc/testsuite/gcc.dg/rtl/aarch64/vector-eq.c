/* { dg-do compile { target aarch64-*-* } } */
/* { dg-additional-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

/*
** foo:
**	mvni	v0.4s, 0
**	ret
*/
__Uint32x4_t __RTL (startwith ("vregs")) foo (void)
{
(function "foo"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:V4SI <0>) (const_vector:V4SI [(const_int 0) (const_int 0) (const_int 0) (const_int 0)])))
      (cinsn 4 (set (reg:V4SI <1>) (reg:V4SI <0>)))
      (cinsn 5 (set (reg:V4SI <2>)
		    (neg:V4SI (eq:V4SI (reg:V4SI <0>) (reg:V4SI <1>)))))
      (cinsn 6 (set (reg:V4SI v0) (reg:V4SI <2>)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
  (crtl (return_rtx (reg/i:V4SI v0)))
)
}

