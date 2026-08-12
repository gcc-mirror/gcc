/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables these fusion pairs yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_SUB_SEQZ" 2 "sched2" { xfail *-*-* } } } */

/* sub + seqz should fuse.  */
long __RTL (startwith ("sched2"))
test_sub_seqz (void)
{
(function "test_sub_seqz"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (minus:DI (reg:DI a1)
                              (reg:DI a2))))
      (cinsn 4 (set (reg:DI a0)
                    (eq:DI (reg:DI a0)
                           (const_int 0))))
      (cinsn 5 (use (reg/i:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:DI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_sub_seqz"
}

/* subw + snez should fuse.  */
long __RTL (startwith ("sched2"))
test_subw_snez (void)
{
(function "test_subw_snez"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (sign_extend:DI
                      (minus:SI (reg:SI a1)
                                (reg:SI a2)))))
      (cinsn 4 (set (reg:DI a0)
                    (ne:DI (reg:DI a0)
                           (const_int 0))))
      (cinsn 5 (use (reg/i:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:DI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_subw_snez"
}
