/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mexplicit-relocs -O2 -mtune=xt-c9501fdvt -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_POSTINDEX_ST" 1 "sched2" } } */

extern long postindex_symbol;

/* The memory offset and update amount are independent.  This store uses
   the same low relocation as the following update.  */
long __RTL (startwith ("sched2"))
test_postindex_lo_sum_store (void)
{
(function "test_postindex_lo_sum_store"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:DI
                      (lo_sum:DI
                        (reg:DI a0)
                        (symbol_ref:DI ("postindex_symbol")))
                      [0 S8 A64])
                    (reg:DI a2)))
      (cinsn 4 (set (reg:DI a0)
                    (lo_sum:DI
                      (reg:DI a0)
                      (symbol_ref:DI ("postindex_symbol")))))
      (cinsn 5 (use (reg:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_postindex_lo_sum_store"
}
