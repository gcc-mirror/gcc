/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mexplicit-relocs -O2 -mtune=xt-c9501fdvt -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDPREINCREMENT" 2 "sched2" } } */

extern double preindex_symbol;

/* The load may write the address-update destination.  */
long __RTL (startwith ("sched2"))
test_preindex_same_destination (void)
{
(function "test_preindex_same_destination"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (plus:DI (reg:DI a0) (const_int 1))))
      (cinsn 4 (set (reg:DI a0)
                    (zero_extend:DI
                      (mem:QI (reg:DI a0) [0 S1 A8]))))
      (cinsn 5 (use (reg/i:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_preindex_same_destination"
}

/* Both the update and memory address use LO_SUM.  The load intentionally
   applies the low relocation again after the update.  */
long __RTL (startwith ("sched2"))
test_preindex_lo_sum_fload (void)
{
(function "test_preindex_lo_sum_fload"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (lo_sum:DI
                      (reg:DI a0)
                      (symbol_ref:DI ("preindex_symbol")))))
      (cinsn 4 (set (reg:DF fa0)
                    (mem:DF
                      (lo_sum:DI
                        (reg:DI a0)
                        (symbol_ref:DI ("preindex_symbol")))
                      [0 S8 A64])))
      (cinsn 5 (use (reg:DI a0)))
      (cinsn 6 (use (reg:DF fa0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 8)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_preindex_lo_sum_fload"
}
