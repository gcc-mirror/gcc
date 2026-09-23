/* PR rtl-optimization/126184 */
/* { dg-do run } */
/* { dg-options "-O2 --param=max-rtl-if-conversion-unpredictable-cost=100 -fdump-rtl-ce1" } */

/* Mixed-mode hard-register definitions cannot be rewired through a single
   temporary.  Verify that the multi-set conversion rejects the block.  */

unsigned long long __RTL (startwith ("ce1"))
f (unsigned long long c, unsigned long long x, unsigned long long y)
{
(function "f"
  (param "c" (DECL_RTL (reg/v:DI x4 [ c ]))
             (DECL_RTL_INCOMING (reg/v:DI x0 [ c ])))
  (param "x" (DECL_RTL (reg/v:DI <1> [ x ]))
             (DECL_RTL_INCOMING (reg/v:DI x1 [ x ])))
  (param "y" (DECL_RTL (reg/v:DI <2> [ y ]))
             (DECL_RTL_INCOMING (reg/v:DI x2 [ y ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v:DI x4 [ c ]) (reg:DI x0)))
      (cinsn 3 (set (reg/v:DI <1> [ x ]) (reg:DI x1)))
      (cinsn 4 (set (reg/v:DI <2> [ y ]) (reg:DI x2)))
      (cinsn 5 (set (reg:DI <3>)
                    (plus:DI (reg/v:DI <1>) (const_int 11))))
      (cinsn 6 (set (reg:DI <4>)
                    (plus:DI (reg/v:DI <2>) (const_int -2))))
      (cinsn 7 (set (reg:CC cc)
                    (compare:CC (reg/v:DI x4) (const_int 0))))
      (cjump_insn 8 (set (pc)
        (if_then_else (eq (reg:CC cc) (const_int 0))
                      (label_ref 30) (pc))))
      (edge-to 3 (flags "FALLTHRU"))
      (edge-to 4)
    )
    (block 3
      (edge-from 2 (flags "FALLTHRU"))
      (cnote 9 [bb 3] NOTE_INSN_BASIC_BLOCK)
      (cinsn 10 (set (reg/v:DI x4 [ c ])
                     (plus:DI (reg/v:DI <1>) (const_int 1))))
      (cinsn 11 (set (reg:DI <3>)
                     (plus:DI (reg/v:DI x4) (reg/v:DI <2>))))
      (cinsn 12 (set (reg:SI x4) (const_int 26)))
      (cinsn 13 (set (reg:DI <4>)
                     (plus:DI (reg/v:DI x4) (reg/v:DI <1>))))
      (edge-to 4 (flags "FALLTHRU"))
    )
    (block 4
      (edge-from 2)
      (edge-from 3 (flags "FALLTHRU"))
      (clabel 30 2)
      (cnote 31 [bb 4] NOTE_INSN_BASIC_BLOCK)
      (cinsn 32 (set (reg:DI <5>)
                     (plus:DI (reg:DI <3>) (reg:DI <4>))))
      (cinsn 33 (set (reg/i:DI x0) (reg:DI <5>)))
      (cinsn 34 (use (reg/i:DI x0)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
  (crtl (return_rtx (reg/i:DI x0)))
)
}

int
main (void)
{
  if (f (7, 13, 17) != 70)
    __builtin_abort ();
  if (f (0, 13, 17) != 39)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-rtl-dump-not "if-conversion succeeded through noce_convert_multiple_sets" "ce1" } } */
