/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2 -mtune=xt-c9501fdvt -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_LDST_PAIR_INC" "sched2" } } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_LDST_PAIR_DEC" "sched2" } } */

/* Reject unaligned loads and stores, covering both widths and directions.  */
long __RTL (startwith ("sched2"))
test_unaligned_load_store_pairs (void)
{
(function "test_unaligned_load_store_pairs"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (sign_extend:DI
                      (mem:SI
                        (plus:DI (reg:DI a0)
                                 (const_int 2)) [0 S4 A16]))))
      (cinsn 4 (set (reg:DI a2)
                    (sign_extend:DI
                      (mem:SI
                        (plus:DI (reg:DI a0)
                                 (const_int 6)) [0 S4 A16]))))
      (cinsn 5 (set (reg:DI a3)
                    (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 12)) [0 S8 A32])))
      (cinsn 6 (set (reg:DI a4)
                    (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 4)) [0 S8 A32])))
      (cinsn 7 (set (mem:SI
                     (plus:DI (reg:DI a0)
                              (const_int 18)) [0 S4 A16])
                    (reg:SI a5)))
      (cinsn 8 (set (mem:SI
                     (plus:DI (reg:DI a0)
                              (const_int 22)) [0 S4 A16])
                    (reg:SI a6)))
      (cinsn 9 (set (mem:DI
                     (plus:DI (reg:DI a0)
                              (const_int 44)) [0 S8 A32])
                    (reg:DI a5)))
      (cinsn 10 (set (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 36)) [0 S8 A32])
                     (reg:DI a6)))
      (cinsn 11 (use (reg:DI a1)))
      (cinsn 12 (use (reg:DI a2)))
      (cinsn 13 (use (reg:DI a3)))
      (cinsn 14 (use (reg:DI a4)))
      (cjump_insn 15 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 16)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_unaligned_load_store_pairs"
}
