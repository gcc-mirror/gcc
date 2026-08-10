/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O2" } */

/* The peephole2 patterns that build SUBS require the destination to satisfy
   aarch64_general_reg.  Every allocatable general register must satisfy it,
   including x8-x15, which REGNO_REG_CLASS reports as the W8_W11_REGS and
   W12_W15_REGS subclasses rather than as GENERAL_REGS.  */

int __RTL (startwith ("peephole2")) sub_compare_x2 ()
{
(function "sub_compare_x2"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 90 (set (reg:CC cc)
		 (compare:CC (reg:DI x2) (reg:DI x5))))
      (cinsn 89 (set (reg:DI x2)
		 (minus:DI (reg:DI x2) (reg:DI x5))))
      ;; Extra insns to avoid the above being deleted by DCE.
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 11 (use (reg/i:DI x2)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "sub_compare_x2"
}

int __RTL (startwith ("peephole2")) sub_compare_x9 ()
{
(function "sub_compare_x9"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 90 (set (reg:CC cc)
		 (compare:CC (reg:DI x2) (reg:DI x5))))
      (cinsn 89 (set (reg:DI x9)
		 (minus:DI (reg:DI x2) (reg:DI x5))))
      ;; Extra insns to avoid the above being deleted by DCE.
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 11 (use (reg/i:DI x9)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "sub_compare_x9"
}

int __RTL (startwith ("peephole2")) sub_compare_x13 ()
{
(function "sub_compare_x13"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 90 (set (reg:CC cc)
		 (compare:CC (reg:DI x2) (reg:DI x5))))
      (cinsn 89 (set (reg:DI x13)
		 (minus:DI (reg:DI x2) (reg:DI x5))))
      ;; Extra insns to avoid the above being deleted by DCE.
      (cinsn 12 (use (reg/i:DI cc)))
      (cinsn 11 (use (reg/i:DI x13)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "sub_compare_x13"
}

/* { dg-final { scan-assembler-times {subs\tx[0-9]+, x2, x5} 3 } } */
/* { dg-final { scan-assembler-not {\tcmp\t} } } */
