/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables these fusion pairs yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ANDI_ADD" 2 "sched2" { xfail *-*-* } } } */

/* andi feeding add.uw's zero-extended operand should fuse.  */
long __RTL (startwith ("sched2"))
test_andi_adduw (void)
{
(function "test_andi_adduw"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (and:DI (reg:DI a1)
                            (const_int -3))))
      (cinsn 4 (set (reg:DI a0)
                    (plus:DI (zero_extend:DI (reg:SI a0))
                             (reg:DI a2))))
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
) ;; function "test_andi_adduw"
}

/* An add may use andi's result as both sources.  */
long __RTL (startwith ("sched2"))
test_andi_add_same_source (void)
{
(function "test_andi_add_same_source"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (and:DI (reg:DI a1)
                            (const_int 85))))
      (cinsn 4 (set (reg:DI a0)
                    (plus:DI (reg:DI a0)
                             (reg:DI a0))))
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
) ;; function "test_andi_add_same_source"
}
