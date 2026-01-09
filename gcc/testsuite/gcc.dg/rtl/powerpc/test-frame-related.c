/* { dg-do compile { target { powerpc64*-*-linux* } } } */
/* { dg-options "-O2 -mregnames -mpowerpc64" } */

int __RTL (startwith ("cprop_hardreg")) test_frame_related ()
{
(function "frame_related"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 8 (set (reg:DI %r5)
                    (reg:DI lr)))
      (cinsn/f 9 (set (reg:DI %r0)
                    (reg:DI lr)))
      (cinsn/f 10 (set (mem/c:DI (plus:DI (reg/f:DI %r1)
                            (const_int 16 ))[18  S8 A8])
                    (reg:DI %r0)))

      ;; Extra insn to avoid the above being deleted by DCE.
      (cinsn 18 (use (reg:DI %r5)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function "main"
}

/* { dg-final { scan-assembler {\mmflr %r0\M}  } } */
/* { dg-final { scan-assembler {\mmflr %r5\M}  } } */
/* { dg-final { scan-assembler {\mstd %r0,16\(%r1\)}  } } */
