/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LOGIC_LOGIC" 1 "sched2" { xfail *-*-* } } } */

extern long fusion_low_symbol;

/* andi expressed as zero_extend followed by ori should fuse.  */
long __RTL (startwith ("sched2"))
test_zero_extend_andi_ori (void)
{
(function "test_zero_extend_andi_ori"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (zero_extend:DI
                      (subreg:QI (reg:DI a1) 0))))
      (cinsn 4 (set (reg:DI a0)
                    (ior:DI (reg:DI a0)
                            (const_int 16))))
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
) ;; function "test_zero_extend_andi_ori"
}
