/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2 -mtune=xt-c9501fdvt -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDST_PAIR_INC" 1 "sched2" } } */

/* The second load destination may be the shared base register.  Offsets
   aligned to the access size need not be aligned to twice that size.  */
long __RTL (startwith ("sched2"))
test_load_pair_inc_second_destination_is_base (void)
{
(function "test_load_pair_inc_second_destination_is_base"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 8)) [0 S8 A64])))
      (cinsn 4 (set (reg:DI a0)
                    (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 16)) [0 S8 A64])))
      (cinsn 5 (use (reg:DI a1)))
      (cinsn 6 (use (reg:DI a0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 8)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_load_pair_inc_second_destination_is_base"
}

/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_LDST_PAIR_DEC" "sched2" } } */
