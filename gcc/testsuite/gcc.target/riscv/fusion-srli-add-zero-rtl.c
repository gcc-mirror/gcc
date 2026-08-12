/* Preserve zero shifts through sched2 to check the lower bound.  */
/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times {(?s);; Function test_srli(?:_add|w_addw)_zero (?:(?!;; Function).)*RISCV_FUSE_SRLI_ADD} 2 "sched2" } } */

long __RTL (startwith ("sched2"))
test_srli_add_zero (void)
{
(function "test_srli_add_zero"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (lshiftrt:DI (reg:DI a1)
                                 (const_int 0))))
      (cinsn 4 (set (reg:DI a0)
                    (plus:DI (reg:DI a0)
                             (reg:DI a2))))
      (cinsn 5 (use (reg/i:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit))
    (cbarrier 7))
  (crtl (return_rtx (reg/i:DI a0))))
}

long __RTL (startwith ("sched2"))
test_srliw_addw_zero (void)
{
(function "test_srliw_addw_zero"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (sign_extend:DI
                      (lshiftrt:SI (reg:SI a1)
                                   (const_int 0)))))
      (cinsn 4 (set (reg:DI a0)
                    (sign_extend:DI
                      (plus:SI (reg:SI a0)
                               (reg:SI a2)))))
      (cinsn 5 (use (reg/i:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit))
    (cbarrier 7))
  (crtl (return_rtx (reg/i:DI a0))))
}
