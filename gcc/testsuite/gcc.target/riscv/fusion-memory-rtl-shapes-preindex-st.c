/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_PREINDEX_ST" 1 "sched2" { xfail *-*-* } } } */

/* Pre-index update followed by a store of the zero register should fuse.  */
long __RTL (startwith ("sched2"))
test_preindex_store_zero (void)
{
(function "test_preindex_store_zero"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (plus:DI (reg:DI a0)
                             (const_int 8))))
      (cinsn 4 (set (mem:DI (reg:DI a0) [0  S8 A64])
                    (const_int 0)))
      (cinsn 5 (use (reg:DI a0)))
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
) ;; function "test_preindex_store_zero"
}
