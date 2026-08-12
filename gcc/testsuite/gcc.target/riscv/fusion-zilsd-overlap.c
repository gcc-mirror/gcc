/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv32i_zilsd -mabi=ilp32 -O2" } */
/* { dg-additional-options "-mtune=xt-c9501fdvt" } */
/* { dg-additional-options "-fdump-rtl-sched2-details" } */

/* Zilsd accesses use two GPRs.  The load writes a0/a1, so it must
   not fuse with a second load using a1 as the base.  The stores read
   a0/a1, so they must not fuse with an address update writing a1.  */

int __RTL (startwith ("sched2"))
load_pair_overlap (void)
{
(function "load_pair_overlap"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a0)
                    (mem:DI (reg:SI a1) [0 S8 A64])))
      (cinsn 4 (set (reg:DI a2)
                    (mem:DI
                      (plus:SI (reg:SI a1)
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
add_store_overlap (void)
{
(function "add_store_overlap"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:SI a1)
                    (plus:SI (reg:SI a4)
                             (reg:SI a5))))
      (cinsn 4 (set (mem:DI (reg:SI a1) [0 S8 A64])
                    (reg:DI a0)))
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
preindex_store_overlap (void)
{
(function "preindex_store_overlap"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:SI a1)
                    (plus:SI (reg:SI a1)
                             (const_int 16))))
      (cinsn 4 (set (mem:DI (reg:SI a1) [0 S8 A64])
                    (reg:DI a0)))
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
postindex_store_overlap (void)
{
(function "postindex_store_overlap"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:DI (reg:SI a1) [0 S8 A64])
                    (reg:DI a0)))
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

/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_LDST_PAIR_INC" "sched2" } } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_ADD_ST" "sched2" } } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_PREINDEX_ST" "sched2" } } */
/* { dg-final { scan-rtl-dump-not "RISCV_FUSE_POSTINDEX_ST" "sched2" } } */
