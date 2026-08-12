/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_SLLI_SRLI" 2 "sched2" { xfail *-*-* } } } */

/* Raw SImode slli followed by raw SImode srli should fuse.  */
int __RTL (startwith ("sched2"))
test_raw_slliw_srliw (void)
{
(function "test_raw_slliw_srliw"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:SI a0)
                    (ashift:SI (reg:SI a1)
                               (const_int 4))))
      (cinsn 4 (set (reg:SI a0)
                    (lshiftrt:SI (reg:SI a0)
                                 (const_int 8))))
      (cinsn 5 (use (reg/i:SI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:SI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_raw_slliw_srliw"
}
/* Rotate-and-mask slliw followed by a sign-bit srliw should fuse.  */
long __RTL (startwith ("sched2"))
test_slliw_srliw_31 (void)
{
(function "test_slliw_srliw_31"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (sign_extend:DI
                      (and:SI
                        (rotatert:SI (reg:SI a1)
                                     (const_int 28))
                        (const_int -16)))))
      (cinsn 4 (set (reg:DI a0)
                    (lt:DI (reg:SI a0)
                           (const_int 0))))
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
) ;; function "test_slliw_srliw_31"
}
