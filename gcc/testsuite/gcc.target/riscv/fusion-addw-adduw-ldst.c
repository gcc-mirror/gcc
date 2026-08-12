/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDINDEXED" 2 "sched2" } } */

/* addw + ld should fuse.  */
long __RTL (startwith ("sched2"))
test_addw_ld (void)
{
(function "test_addw_ld"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      ;; addw a0, a1, a2
      (cinsn 3 (set (reg:DI a0)
                    (sign_extend:DI
                      (plus:SI (reg:SI a1)
                               (reg:SI a2)))))
      ;; ld a1, 0(a0)
      (cinsn 4 (set (reg:DI a1)
                    (mem:DI (reg:DI a0) [0  S8 A64])))
      (cinsn 5 (set (reg:DI a0) (reg:DI a1)))
      (cinsn 6 (use (reg/i:DI a0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 8)
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:DI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_addw_ld"
}

/* add.uw + ld should fuse.  */
long __RTL (startwith ("sched2"))
test_adduw_ld (void)
{
(function "test_adduw_ld"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      ;; add.uw a0, a1, a2
      (cinsn 3 (set (reg:DI a0)
                    (plus:DI (zero_extend:DI (reg:SI a1))
                             (reg:DI a2))))
      ;; ld a1, 0(a0)
      (cinsn 4 (set (reg:DI a1)
                    (mem:DI (reg:DI a0) [0  S8 A64])))
      (cinsn 5 (set (reg:DI a0) (reg:DI a1)))
      (cinsn 6 (use (reg/i:DI a0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 8)
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:DI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "test_adduw_ld"
}
