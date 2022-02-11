/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O3 -mbig-endian" } */

void __RTL (startwith ("vregs")) foo (void *ptr1, void *ptr2)
{
  (function "foo"
    (param "ptr1"
      (DECL_RTL (reg/v:DI <0> [ ptr1 ]))
      (DECL_RTL_INCOMING (reg:DI x0 [ ptr1 ]))
    ) ;; param "ptr1"
    (param "ptr2"
      (DECL_RTL (reg/v:DI <1> [ ptr2 ]))
      (DECL_RTL_INCOMING (reg:DI x1 [ ptr2 ]))
    ) ;; param "ptr2"
    (insn-chain
      (block 2
	(edge-from entry (flags "FALLTHRU"))
	(cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
	(insn 4 (set (reg:DI <0>) (reg:DI x0)))
	(insn 5 (set (reg:DI <1>) (reg:DI x1)))
	(insn 6 (set (reg:V2SI <2>)
		     (const_vector:V2SI [(const_int 1)
					 (const_int 0)])) (nil))
	(insn 7 (set (mem:V2SI (reg:DI <0>) [1 ptr1+0 S8 A8])
		     (reg:V2SI <2>)))
	(insn 8 (set (reg:V4SI <3>)
		     (const_vector:V4SI [(const_int 1)
					 (const_int 1)
					 (const_int 1)
					 (const_int 1)])) (nil))
	(insn 9 (set (reg:SI <4>) (subreg:SI (reg:V4SI <3>) 12))
		(expr_list:REG_EQUAL (const_int 1) (nil)))
	(insn 10 (set (mem:SI (reg:DI <1>) [1 ptr2+0 S4 A4])
		      (reg:SI <4>)))
	(edge-to exit (flags "FALLTHRU"))
      ) ;; block 2
    ) ;; insn-chain
  ) ;; function
}

/* { dg-final { scan-assembler-not {\tstr\twzr,} } } */
