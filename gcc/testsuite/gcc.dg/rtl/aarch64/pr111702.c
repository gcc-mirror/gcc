/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2" } */

extern int data[];

void __RTL (startwith ("vregs")) foo ()
{
  (function "foo"
    (insn-chain
      (block 2
	(edge-from entry (flags "FALLTHRU"))
	(cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
	(insn 4 (set (reg:V16QI <0>)
		     (const_vector:V16QI [(const_int 1) (const_int 1)
					  (const_int 0) (const_int 0)
					  (const_int 1) (const_int 1)
					  (const_int 0) (const_int 0)
					  (const_int 1) (const_int 1)
					  (const_int 0) (const_int 0)
					  (const_int 1) (const_int 1)
					  (const_int 0) (const_int 0)])))
	(insn 5 (set (reg:V2SI v0)
		     (const_vector:V2SI [(const_int 1) (const_int 0)])))
	(insn 6 (set (reg:V16QI v1)
		     (const_vector:V16QI [(const_int 0) (const_int 0)
					  (const_int 1) (const_int 1)
					  (const_int 0) (const_int 0)
					  (const_int 1) (const_int 1)
					  (const_int 0) (const_int 0)
					  (const_int 1) (const_int 1)
					  (const_int 0) (const_int 0)
					  (const_int 1) (const_int 1)])))
	(insn 7 (set (reg:QI x0) (subreg:QI (reg:V16QI <0>) 0))
		(expr_list:REG_EQUAL (const_int 1) (nil)))
	(insn 8 (use (reg:V16QI <0>)))
	(insn 9 (use (reg:V2SI v0)))
	(insn 10 (use (reg:V16QI v1)))
	(insn 11 (use (reg:QI x0)))
	(edge-to exit (flags "FALLTHRU"))
      ) ;; block 2
    ) ;; insn-chain
  ) ;; function
}
