/* Verify that macro fusion does not cause an ICE in selective scheduling.  */

/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-O2 -fselective-scheduling2 -fsel-sched-pipelining" } */
/* { dg-additional-options "-march=rv64gc -mabi=lp64d" } */
/* { dg-additional-options "-mtune=xiangshan-nanhu -fchecking=2" } */

long __RTL (startwith ("sched2"))
sched_fusion_selective_ice (void)
{
(function "sched_fusion_selective_ice"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (edge-to 3 (flags "FALLTHRU"))
    )
    (block 3
      (edge-from 2 (flags "FALLTHRU"))
      (edge-from 3)
      (clabel 3 2)
      (cnote 4 [bb 3] NOTE_INSN_BASIC_BLOCK)
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
      (cinsn 12 (set (reg:DI a3)
                     (plus:DI (reg:DI a3) (reg:DI a0))))
      (cjump_insn 13 (set (pc)
                         (if_then_else (ne (reg:DI a3) (const_int 0))
                                       (label_ref 3) (pc))))
      (edge-to 3)
      (edge-to 4 (flags "FALLTHRU"))
    )
    (block 4
      (edge-from 3 (flags "FALLTHRU"))
      (cnote 14 [bb 4] NOTE_INSN_BASIC_BLOCK)
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
