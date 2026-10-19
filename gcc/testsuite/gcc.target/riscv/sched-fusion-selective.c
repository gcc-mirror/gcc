/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-O2 -fselective-scheduling2 -fdump-rtl-sched2" } */
/* { dg-additional-options "-march=rv64gc -mabi=lp64d" } */
/* { dg-additional-options "-mtune=xiangshan-nanhu -fchecking=2" } */
/* { dg-final { scan-rtl-dump "RISCV_FUSE_ZEXTW" "sched2" } } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_ZEXTH" "sched2" } } */

long __RTL (startwith ("sched2"))
sched_fusion_selective (void)
{
(function "sched_fusion_selective"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 5 (set (reg:DI a2)
                    (ashift:DI (reg:DI a2) (const_int 48))))
      (cinsn 6 (set (reg:DI a1)
                    (mult:DI (reg:DI a1) (reg:DI a4))))
      (cinsn 7 (set (reg:DI a2)
                    (lshiftrt:DI (reg:DI a2) (const_int 48))))
      (cinsn 8 (set (reg:DI a0)
                    (ashift:DI (reg:DI a0) (const_int 32))))
      (cinsn 9 (set (reg:DI a0)
                    (lshiftrt:DI (reg:DI a0) (const_int 32))))
      (cinsn 10 (set (reg:DI a0)
                     (plus:DI (reg:DI a0) (reg:DI a1))))
      (cinsn 11 (set (reg:DI a0)
                     (plus:DI (reg:DI a0) (reg:DI a2))))
      (cinsn 15 (use (reg:DI a0)))
      (cjump_insn 16 (simple_return))
      (edge-to exit)
    )
    (cbarrier 17)
  )
  (crtl
    (return_rtx (reg/i:DI a0))
  )
)
}
