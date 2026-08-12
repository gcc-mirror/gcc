/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2 -mtune=xt-c9501fdvt -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDINDEXED" 1 "sched2" { xfail *-*-* } } } */

/* add and load writing the same register should fuse.  */
long __RTL (startwith ("sched2"))
test_add_load_same_destination (void)
{
(function "test_add_load_same_destination"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (plus:DI (reg:DI a1)
                             (reg:DI a2))))
      (cinsn 4 (set (reg:DI a0)
                    (mem:DI (reg:DI a0) [0  S8 A64])))
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
) ;; function "test_add_load_same_destination"
}
