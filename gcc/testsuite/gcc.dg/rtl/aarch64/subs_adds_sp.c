/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O2" } */
/*
   Tests are:
      Patterns allow subs/adds with a stack pointer source.
      define_peephole2's don't generate patterns for subs/adds with a stack
      pointer destination.
 */

/* These functions used to ICE due to using the stack pointer as a source
   register.  */

int __RTL (startwith ("final"))
adds ()
{
(function "adds"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (parallel [
	    (set (reg:CC cc)
		(compare:CC (reg/f:DI sp)
		    (const_int -3)))
	    (set (reg/f:DI x19)
		(plus:DI (reg/f:DI sp)
		    (const_int 3)))
	]))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 10 (use (reg/i:SI x19)))
      (cinsn 11 (use (reg/i:SI sp)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

int __RTL (startwith ("final"))
subs ()
{
(function "subs"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 101 (parallel [
	    (set (reg:CC cc)
		(compare:CC (reg/f:DI sp)
		    (const_int 3)))
	    (set (reg/f:DI x19)
		(plus:DI (reg/f:DI sp)
		    (const_int -3)))
	]))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 10 (use (reg/i:SI x19)))
      (cinsn 11 (use (reg/i:SI sp)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

/* These functions used to trigger peepholes generating invalid SUBS patterns
   that used the stack pointer for the destination register.  */

int __RTL (startwith ("peephole2")) sub3_compare1_peephole_1 ()
{
(function "sub3_compare1_peephole_1"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 89 (set (reg:DI sp)
		 (minus:DI (reg:DI x2) (reg:DI x5))))
      (cinsn 90 (set (reg:CC cc)
		 (compare:CC (reg:DI x2) (reg:DI x5))))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 11 (use (reg/i:DI sp)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

int __RTL (startwith ("peephole2")) sub3_compare1_peephole_2 ()
{
(function "sub3_compare1_peephole_2"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 90 (set (reg:CC cc)
		 (compare:CC (reg:DI x2) (reg:DI x5))))
      (cinsn 89 (set (reg:DI sp)
		 (minus:DI (reg:DI x2) (reg:DI x5))))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 11 (use (reg/i:DI sp)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

int __RTL (startwith ("peephole2")) sub3_compare1_imm_peephole_1 ()
{
(function "sub3_compare1_imm_peephole_1"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 90 (set (reg:CC cc)
		 (compare:CC (reg:DI x2) (reg:DI x5))))
      (cinsn 89 (set (reg:DI sp)
		 (minus:DI (reg:DI x2) (reg:DI x5))))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 11 (use (reg/i:DI sp)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

int __RTL (startwith ("peephole2")) sub3_compare1_imm_peephole_2 ()
{
(function "sub3_compare1_imm_peephole_1"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 89 (set (reg:DI sp)
		 (minus:DI (reg:DI x2) (reg:DI x5))))
      (cinsn 90 (set (reg:CC cc)
		 (compare:CC (reg:DI x2) (reg:DI x5))))
      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 11 (use (reg/i:DI sp)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

/* Verify that the adds and subs functions generated their respective
   instructions, and that none of the other functions generated either since
   they are setting the stack pointer.  */
/* { dg-final { scan-assembler-times {adds\tx[0-9]+, sp} 1 } }  */
/* { dg-final { scan-assembler-not {adds\tsp} } }  */
/* { dg-final { scan-assembler-times {subs\tx[0-9]+, sp} 1 } }  */
/* { dg-final { scan-assembler-not {subs\tsp} } }  */

