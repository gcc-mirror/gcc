/* A LUI result must feed ADDI even before register allocation.  */
/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O2 -fno-dce -mcmodel=medany -mexplicit-relocs -mtune=sifive-p600-series -fdump-rtl-sched1-details" } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_LUI_ADDI" "sched1" } } */

long __RTL (startwith ("sched1"))
test_lui_addi_source_mismatch (void)
{
(function "test_lui_addi_source_mismatch"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (const_int 305418240)))
      (cinsn 4 (set (reg:DI a0)
                    (plus:DI (reg:DI a1)
                             (const_int 1656))))
      (cinsn 5 (use (reg:DI a0)))
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
) ;; function "test_lui_addi_source_mismatch"
}
