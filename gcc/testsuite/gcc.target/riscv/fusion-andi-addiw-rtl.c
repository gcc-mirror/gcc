/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2 -mtune=xt-c9501fdvt -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ANDI_ADD" 1 "sched2" } } */
/* { dg-final { scan-assembler {addiw\s+a0,a0,5} } } */

/* Supply ADDIW directly so that earlier passes cannot replace it with ADDI.  */
long __RTL (startwith ("sched2"))
test_andi_addiw (void)
{
(function "test_andi_addiw"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (and:DI (reg:DI a0)
                            (const_int 85))))
      (cinsn 4 (set (reg:DI a0)
                    (sign_extend:DI
                      (plus:SI (reg:SI a0)
                               (const_int 5)))))
      (cinsn 5 (use (reg/i:DI a0)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "test_andi_addiw"
}
