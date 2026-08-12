/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables these fusion pairs yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ADD_ANDI" 2 "sched2" { xfail *-*-* } } } */

/* mv followed by andi should fuse.  */
long __RTL (startwith ("sched2"))
test_mv_andi (void)
{
(function "test_mv_andi"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (reg:DI a1)))
      (cinsn 4 (set (reg:DI a0)
                    (and:DI (reg:DI a0)
                            (const_int 85))))
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
) ;; function "test_mv_andi"
}

/* li followed by andi should fuse.  */
long __RTL (startwith ("sched2"))
test_li_andi (void)
{
(function "test_li_andi"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (const_int 31)))
      (cinsn 4 (set (reg:DI a0)
                    (and:DI (reg:DI a0)
                            (const_int 85))))
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
) ;; function "test_li_andi"
}
