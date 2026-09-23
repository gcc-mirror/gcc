/* { dg-do compile { target aarch64-*-* } } */
/* { dg-options "-O2 -fdump-rtl-ce1" } */

/* PR125683: when ce1 collapses "c ? a : a" the folded MEM must not claim
   anything only one arm guaranteed.  MEM_READONLY_P and MEM_NOTRAP_P
   disagreement cannot be produced from C source -- both are derived from
   the base object, which two loads from the same address share -- so the
   two MEMs are built here directly.  The else arm is the one the fold
   copies, so it is the arm that carries the flags.

   Based on the ce1 input for

     long f (int a, void *cc, long *d)
     { long c; *d = 0; if (a) c = *(long *) cc; else c = *(long *) cc;
       *d = 1; return c; }  */

long __RTL (startwith ("ce1")) f (int a, void *cc, long *d)
{
(function "f"
  (param "a"
    (DECL_RTL (reg/v:SI <3> [ a ]))
    (DECL_RTL_INCOMING (reg:SI x0 [ a ])))
  (param "cc"
    (DECL_RTL (reg/v/f:DI <4> [ cc ]))
    (DECL_RTL_INCOMING (reg:DI x1 [ cc ])))
  (param "d"
    (DECL_RTL (reg/v/f:DI <5> [ d ]))
    (DECL_RTL_INCOMING (reg:DI x2 [ d ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 6 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v:SI <3> [ a ])
                    (reg:SI x0 [ a ])))
      (cinsn 3 (set (reg/v/f:DI <4> [ cc ])
                    (reg:DI x1 [ cc ])))
      (cinsn 4 (set (reg/v/f:DI <5> [ d ])
                    (reg:DI x2 [ d ])))
      (cnote 5 NOTE_INSN_FUNCTION_BEG)
      (cinsn 8 (set (mem:DI (reg/v/f:DI <5> [ d ]) [3  S8 A64])
                    (const_int 0)))
      (cinsn 9 (set (reg:CC cc)
                    (compare:CC (reg/v:SI <3> [ a ])
                        (const_int 0))))
      (cjump_insn 10 (set (pc)
                    (if_then_else (eq (reg:CC cc)
                            (const_int 0))
                        (label_ref 15)
                        (pc))))
      (edge-to 3 (flags "FALLTHRU"))
      (edge-to 4)
    ) ;; block 2
    (block 3
      (edge-from 2 (flags "FALLTHRU"))
      (cnote 11 [bb 3] NOTE_INSN_BASIC_BLOCK)
      (cinsn 12 (set (reg/v:DI <2>)
                    (mem:DI (reg/v/f:DI <4> [ cc ]) [2  S8 A64])))
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 3
    (block 4
      (edge-from 2)
      (clabel 15 2)
      (cnote 16 [bb 4] NOTE_INSN_BASIC_BLOCK)
      (cinsn 17 (set (reg/v:DI <2>)
                    (mem/u/c/f:DI (reg/v/f:DI <4> [ cc ]) [1  S8 A64])))
      (edge-to 5 (flags "FALLTHRU"))
    ) ;; block 4
    (block 5
      (edge-from 3 (flags "FALLTHRU"))
      (edge-from 4 (flags "FALLTHRU"))
      (cnote 20 [bb 5] NOTE_INSN_BASIC_BLOCK)
      (cinsn 19 (set (reg:DI <6>)
                    (const_int 1)))
      (cinsn 21 (set (mem:DI (reg/v/f:DI <5> [ d ]) [3  S8 A64])
                    (reg:DI <6>)))
      (cinsn 22 (set (reg/i:DI x0)
                    (reg/v:DI <2>)))
      (cinsn 23 (use (reg/i:DI x0)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 5
  ) ;; insn-chain
) ;; function
}

/* The diamond must actually be collapsed, otherwise the rest proves
   nothing.  */
/* { dg-final { scan-rtl-dump "noce_try_ifelse_collapse" "ce1" } } */

/* The alias sets disagree, so the fold drops to alias set 0.  That MEM is
   the folded one; it must carry no flag bits, since each was set on only
   one arm.  Before the flags were handled it came out as
   "(mem/u/c:DI (reg...) [0  S8 A64])".  The dump also lists the pass input,
   where the /u/c arm legitimately appears, so both checks are anchored on
   the alias set 0 that only the folded MEM has.  */
/* { dg-final { scan-rtl-dump "mem:DI \\(reg\[^\)\]*\\) \\\[0 " "ce1" } } */
/* { dg-final { scan-rtl-dump-not "mem/\[a-z/\]*:DI \\(reg\[^\)\]*\\) \\\[0 " "ce1" } } */
