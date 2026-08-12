/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv32i_zilsd -mabi=ilp32 -O2" } */
/* { dg-additional-options "-mtune=xt-c9501fdvt" } */
/* { dg-additional-options "-fdump-rtl-sched2-details" } */

/* Keep recognizing Zilsd accesses when both registers are independent
   of the base or address update.  */

int __RTL (startwith ("sched2"))
load_pair_independent (void)
{
(function "load_pair_independent"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (mem:DI (reg:SI a5) [0 S8 A64])))
      (cinsn 4 (set (reg:DI a2)
                    (mem:DI
                      (plus:SI (reg:SI a5)
                               (const_int 8)) [0 S8 A64])))
      (cinsn 5 (use (reg:DI a0)))
      (cinsn 6 (use (reg:DI a2)))
      (cjump_insn 7 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 8)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:SI a0))
  ) ;; crtl
)
}

int __RTL (startwith ("sched2"))
add_store_independent (void)
{
(function "add_store_independent"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:SI a1)
                    (plus:SI (reg:SI a4)
                             (reg:SI a5))))
      (cinsn 4 (set (mem:DI (reg:SI a1) [0 S8 A64])
                    (reg:DI a2)))
      (cinsn 5 (use (reg:SI a1)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:SI a0))
  ) ;; crtl
)
}

int __RTL (startwith ("sched2"))
preindex_store_independent (void)
{
(function "preindex_store_independent"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:SI a1)
                    (plus:SI (reg:SI a1)
                             (const_int 16))))
      (cinsn 4 (set (mem:DI (reg:SI a1) [0 S8 A64])
                    (reg:DI a2)))
      (cinsn 5 (use (reg:SI a1)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:SI a0))
  ) ;; crtl
)
}

int __RTL (startwith ("sched2"))
postindex_store_independent (void)
{
(function "postindex_store_independent"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:DI (reg:SI a1) [0 S8 A64])
                    (reg:DI a2)))
      (cinsn 4 (set (reg:SI a1)
                    (plus:SI (reg:SI a1)
                             (const_int 16))))
      (cinsn 5 (use (reg:SI a1)))
      (cjump_insn 6 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 7)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:SI a0))
  ) ;; crtl
)
}

/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDST_PAIR_INC" 1 "sched2" } } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ADD_ST" 1 "sched2" } } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_PREINDEX_ST" 1 "sched2" } } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_POSTINDEX_ST" 1 "sched2" } } */
