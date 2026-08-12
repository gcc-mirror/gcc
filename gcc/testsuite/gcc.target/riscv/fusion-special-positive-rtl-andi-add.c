/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ANDI_ADD" 2 "sched2" { xfail *-*-* } } } */

extern long fusion_low_symbol;

/* andi expressed as zero_extend followed by add should fuse.  */
long __RTL (startwith ("sched2"))
test_zero_extend_andi_add (void)
{
(function "test_zero_extend_andi_add"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (zero_extend:DI
                      (subreg:QI (reg:DI a1) 0))))
      (cinsn 4 (set (reg:DI a0)
                    (plus:DI (reg:DI a0)
                             (reg:DI a2))))
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
) ;; function "test_zero_extend_andi_add"
}
/* andi followed by a lo_sum update should fuse.  */
long __RTL (startwith ("sched2"))
test_andi_lo_sum (void)
{
(function "test_andi_lo_sum"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (and:DI (reg:DI a1)
                            (const_int 85))))
      (cinsn 4 (set (reg:DI a0)
                    (lo_sum:DI
                      (reg:DI a0)
                      (symbol_ref:DI ("fusion_low_symbol")))))
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
) ;; function "test_andi_lo_sum"
}
