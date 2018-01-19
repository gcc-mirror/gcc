/* { dg-do compile { target arm*-*-* } } */
/* { dg-require-effective-target arm_arm_ok } */
/* { dg-require-effective-target arm_arch_v8a_ok } */
/* { dg-options "-O2 -marm" } */
/* { dg-add-options arm_arch_v8a } */

/* We want to test that the STL instruction gets the conditional
   suffix when under a COND_EXEC.  However, COND_EXEC is very hard to
   generate from C code because the atomic_store expansion adds a compiler
   barrier before the insn, preventing if-conversion.  So test the output
   here with a hand-crafted COND_EXEC wrapped around an STL.  */

void __RTL (startwith ("final")) foo (int *a, int b)
{
(function "foo"
  (param "a"
    (DECL_RTL (reg/v:SI r0))
    (DECL_RTL_INCOMING (reg:SI r0))
  )
  (param "b"
    (DECL_RTL (reg/v:SI r1))
    (DECL_RTL_INCOMING (reg:SI r1))
  )
  (insn-chain
    (block 2
	(edge-from entry (flags "FALLTHRU"))
	(cnote 5 [bb 2] NOTE_INSN_BASIC_BLOCK)

  (insn:TI 7 (parallel [
	(set (reg:CC cc)
	     (compare:CC (reg:SI r1)
			 (const_int 0)))
	(set (reg/v:SI r1)
	     (reg:SI r1 ))
        ])  ;; {*movsi_compare0}
     (nil))

  ;; A conditional atomic store-release: STLNE for Armv8-A.
  (insn 10 (cond_exec (ne (reg:CC cc)
	   (const_int 0))
	(set (mem/v:SI (reg/v/f:SI r0) [-1  S4 A32])
		(unspec_volatile:SI [
		(reg/v:SI r1)
		(const_int 3)
		] VUNSPEC_STL))) ;; {*p atomic_storesi}
	(expr_list:REG_DEAD (reg:CC cc)
	(expr_list:REG_DEAD (reg/v:SI r1)
	(expr_list:REG_DEAD (reg/v/f:SI r0)
		(nil)))))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI r0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function
}

/* { dg-final { scan-assembler "stlne" } } */
