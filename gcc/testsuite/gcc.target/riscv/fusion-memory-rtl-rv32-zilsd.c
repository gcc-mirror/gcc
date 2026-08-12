/* { dg-do compile { target { rv32 } } } */
/* { dg-skip-if "RTL input" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv32gc_zilsd -mabi=ilp32d -mtune=xt-c9501fdvt -O2 -fschedule-insns2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDST_PAIR_INC" 1 "sched2" } } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_LDST_PAIR_DEC" "sched2" } } */

/* The first DI destination includes a1 as its high half.  The second load
   uses the updated base, so these accesses must not match a fusion pair.  */
long long __RTL (startwith ("sched2"))
first_destination_high_half_is_base (void)
{
(function "first_destination_high_half_is_base"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
		    (mem:DI (reg:SI a1) [0 S8 A64])))
      (cinsn 4 (set (reg:DI a4)
		    (mem:DI (plus:SI (reg:SI a1) (const_int 8))
		      [0 S8 A64])))
      (cinsn 5 (use (reg:DI a0)))
      (cinsn 6 (use (reg:DI a4)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    )
    (cbarrier 8)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}

/* Only the second destination's high half is the base.  The original
   ascending order is valid and must account for the one fusion match.  */
long long __RTL (startwith ("sched2"))
second_destination_high_half_is_base (void)
{
(function "second_destination_high_half_is_base"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a4)
		    (mem:DI (reg:SI a1) [0 S8 A64])))
      (cinsn 4 (set (reg:DI a0)
		    (mem:DI (plus:SI (reg:SI a1) (const_int 8))
		      [0 S8 A64])))
      (cinsn 5 (use (reg:DI a4)))
      (cinsn 6 (use (reg:DI a0)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    )
    (cbarrier 8)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}
