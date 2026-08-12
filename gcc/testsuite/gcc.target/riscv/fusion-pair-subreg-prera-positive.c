/* { dg-do compile { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv32gc -mabi=ilp32d -O2 -mtune=xt-c9501fdvt -fdump-rtl-sched1-details -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDST_PAIR_INC" 2 "sched1" } } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDST_PAIR_INC" 2 "sched2" } } */

/* Matching low or high parts of the same pseudo remain valid bases.  */
int __RTL (startwith ("sched1"))
test_pair_same_lowpart (void)
{
(function "test_pair_same_lowpart"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI <0>) (reg:DI a0)))
      (cinsn 4 (set (reg:SI <1>)
                    (mem:SI
                      (subreg:SI (reg:DI <0>) 0) [0 S4 A32])))
      (cinsn 5 (set (reg:SI <2>)
                    (mem:SI
                      (plus:SI (subreg:SI (reg:DI <0>) 0)
                               (const_int 4)) [0 S4 A32])))
      (cinsn 6 (set (reg:SI a0)
                    (plus:SI (reg:SI <1>) (reg:SI <2>))))
      (cinsn 7 (use (reg/i:SI a0)))
      (cjump_insn 8 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 9)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:SI a0))
  ) ;; crtl
) ;; function "test_pair_same_lowpart"
}

int __RTL (startwith ("sched1"))
test_pair_same_highpart (void)
{
(function "test_pair_same_highpart"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI <0>) (reg:DI a0)))
      (cinsn 4 (set (reg:SI <1>)
                    (mem:SI
                      (subreg:SI (reg:DI <0>) 4) [0 S4 A32])))
      (cinsn 5 (set (reg:SI <2>)
                    (mem:SI
                      (plus:SI (subreg:SI (reg:DI <0>) 4)
                               (const_int 4)) [0 S4 A32])))
      (cinsn 6 (set (reg:SI a0)
                    (plus:SI (reg:SI <1>) (reg:SI <2>))))
      (cinsn 7 (use (reg/i:SI a0)))
      (cjump_insn 8 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 9)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:SI a0))
  ) ;; crtl
) ;; function "test_pair_same_highpart"
}
