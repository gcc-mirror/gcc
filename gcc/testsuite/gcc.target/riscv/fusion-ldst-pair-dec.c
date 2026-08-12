/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDST_PAIR_DEC" 4 "sched2" } } */

/* Present descending pairs directly to sched2 so that earlier passes do
   not choose the memory order.  Each function has exactly one pair.  */
long __RTL (startwith ("sched2"))
test_ld_pair_dec (long *p)
{
(function "test_ld_pair_dec"
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
                    (mem:DI (reg:DI a0) [0 S8 A64])))
      (cinsn 5 (set (reg:DI a0)
                    (plus:DI (reg:DI a1) (reg:DI a0))))
      (cinsn 6 (use (reg/i:DI a0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 8)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_ld_pair_dec"
}

long __RTL (startwith ("sched2"))
test_lw_pair_dec (int *p)
{
(function "test_lw_pair_dec"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (sign_extend:DI
                      (mem:SI
                        (plus:DI (reg:DI a0)
                                 (const_int 4)) [0 S4 A32]))))
      (cinsn 4 (set (reg:DI a0)
                    (sign_extend:DI
                      (mem:SI (reg:DI a0) [0 S4 A32]))))
      (cinsn 5 (set (reg:DI a0)
                    (plus:DI (reg:DI a1) (reg:DI a0))))
      (cinsn 6 (use (reg/i:DI a0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 8)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_lw_pair_dec"
}

void __RTL (startwith ("sched2"))
test_sd_pair_dec (long *p, long a, long b)
{
(function "test_sd_pair_dec"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 8)) [0 S8 A64])
                    (reg:DI a2)))
      (cinsn 4 (set (mem:DI (reg:DI a0) [0 S8 A64])
                    (reg:DI a1)))
      (cjump_insn 5 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 6)
  ) ;; insn-chain
) ;; function "test_sd_pair_dec"
}

void __RTL (startwith ("sched2"))
test_sw_pair_dec (int *p, int a, int b)
{
(function "test_sw_pair_dec"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:SI
                      (plus:DI (reg:DI a0)
                               (const_int 4)) [0 S4 A32])
                    (reg:SI a2)))
      (cinsn 4 (set (mem:SI (reg:DI a0) [0 S4 A32])
                    (reg:SI a1)))
      (cjump_insn 5 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 6)
  ) ;; insn-chain
) ;; function "test_sw_pair_dec"
}
