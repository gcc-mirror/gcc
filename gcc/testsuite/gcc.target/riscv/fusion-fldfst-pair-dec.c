/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "RTL input" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-split3-slim -fdump-rtl-sched2-details" } */
/* Check the actual input to sched2 even while the tune disables FP pairs.  */
/* { dg-final { scan-rtl-dump {fa1:SF=\[a0:DI\+0x4\]\n\s*\d+: fa0:SF=\[a0:DI\]} "split3" } } */
/* { dg-final { scan-rtl-dump {fa1:DF=\[a0:DI\+0x8\]\n\s*\d+: fa0:DF=\[a0:DI\]} "split3" } } */
/* { dg-final { scan-rtl-dump {\[a0:DI\+0x4\]=fa1:SF\n\s*\d+: \[a0:DI\]=fa0:SF} "split3" } } */
/* { dg-final { scan-rtl-dump {\[a0:DI\+0x8\]=fa1:DF\n\s*\d+: \[a0:DI\]=fa0:DF} "split3" } } */
/* No tune enables RISCV_FUSE_FLDFST_PAIR_DEC yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_FLDFST_PAIR_DEC" 4 "sched2" { xfail *-*-* } } } */

/* Start at the final split before sched2 with one pair per function.  */

float __RTL (startwith ("split3"))
test_flw_pair_dec (const float *p)
{
(function "test_flw_pair_dec"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:SF fa1)
                    (mem:SF (plus:DI (reg:DI a0) (const_int 4))
                            [0 S4 A32])))
      (cinsn 4 (set (reg:SF fa0) (mem:SF (reg:DI a0) [0 S4 A32])))
      (cinsn 5 (use (reg:SF fa1)))
      (cinsn 6 (use (reg:SF fa0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit))
    (cbarrier 8))
  (crtl (return_rtx (reg/i:SF fa0))))
}

double __RTL (startwith ("split3"))
test_fld_pair_dec (const double *p)
{
(function "test_fld_pair_dec"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DF fa1)
                    (mem:DF (plus:DI (reg:DI a0) (const_int 8))
                            [0 S8 A64])))
      (cinsn 4 (set (reg:DF fa0) (mem:DF (reg:DI a0) [0 S8 A64])))
      (cinsn 5 (use (reg:DF fa1)))
      (cinsn 6 (use (reg:DF fa0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit))
    (cbarrier 8))
  (crtl (return_rtx (reg/i:DF fa0))))
}

void __RTL (startwith ("split3"))
test_fsw_pair_dec (float *p, float a, float b)
{
(function "test_fsw_pair_dec"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:SF (plus:DI (reg:DI a0) (const_int 4))
                            [0 S4 A32])
                    (reg:SF fa1)))
      (cinsn 4 (set (mem:SF (reg:DI a0) [0 S4 A32]) (reg:SF fa0)))
      (cjump_insn 5 (simple_return))
      (edge-to exit))
    (cbarrier 6)))
}

void __RTL (startwith ("split3"))
test_fsd_pair_dec (double *p, double a, double b)
{
(function "test_fsd_pair_dec"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:DF (plus:DI (reg:DI a0) (const_int 8))
                            [0 S8 A64])
                    (reg:DF fa1)))
      (cinsn 4 (set (mem:DF (reg:DI a0) [0 S8 A64]) (reg:DF fa0)))
      (cjump_insn 5 (simple_return))
      (edge-to exit))
    (cbarrier 6)))
}
