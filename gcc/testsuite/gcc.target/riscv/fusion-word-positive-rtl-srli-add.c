/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_SRLI_ADD" 2 "sched2" { xfail *-*-* } } } */

/* Raw SImode srli followed by a same-source addw should fuse.  */
long __RTL (startwith ("sched2"))
test_raw_srliw_extended_addw (void)
{
(function "test_raw_srliw_extended_addw"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:SI a0)
                    (lshiftrt:SI (reg:SI a1)
                                 (const_int 2))))
      (cinsn 4 (set (reg:DI a0)
                    (sign_extend:DI
                      (plus:SI (reg:SI a0)
                               (reg:SI a0)))))
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
) ;; function "test_raw_srliw_extended_addw"
}
/* Zero-extended srliw followed by addw expressed via truncate.  */
long __RTL (startwith ("sched2"))
test_lowpart_srliw_addw (void)
{
(function "test_lowpart_srliw_addw"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (zero_extend:DI
                      (lshiftrt:SI (reg:SI a1)
                                   (const_int 2)))))
      (cinsn 4 (set (reg:DI a0)
                    (sign_extend:DI
                      (truncate:SI
                        (plus:DI (reg:DI a0)
                                 (reg:DI a2))))))
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
) ;; function "test_lowpart_srliw_addw"
}
