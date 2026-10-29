/* { dg-do compile } */
/* { dg-options "-O2" } */

/* PR target/122583.  Combine used to ICE trying to truncate this lowpart
   SUBREG of a V4DImode VEC_MERGE, because there is no 256-bit integer mode
   to bit-cast it into.  Previously this was hidden by disallowing packed
   accesses in gcn_vectorize_support_vector_misalignment.  */

void __RTL (startwith ("combine")) f1 (void)
{
(function "f1"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (parallel [(set (reg/f:DI <0>) (symbol_ref:DI ("g")))
			  (clobber (reg:BI scc))]))
      (cinsn 3 (parallel [(set (reg/f:DI <1>) (symbol_ref:DI ("h")))
			  (clobber (reg:BI scc))]))
      (cinsn 4 (parallel [(set (reg/f:DI <2>) (symbol_ref:DI ("r")))
			  (clobber (reg:BI scc))]))
      (cinsn 5 (set (reg:DI <3>) (mem/c:DI (reg/f:DI <0>) [0  S8 A64])))
      (cinsn 6 (set (reg:DI <4>) (mem/c:DI (reg/f:DI <1>) [0  S8 A64])))
      (cinsn 7 (set (reg:V4DI <5>) (vec_duplicate:V4DI (reg:DI <3>))))
      (cinsn 8 (set (reg:V4DI <6>)
		    (vec_merge:V4DI (vec_duplicate:V4DI (reg:DI <4>))
				    (reg:V4DI <5>)
				    (ashift (const_int 1) (const_int 3)))))
      (cinsn 9 (set (reg:SI <7>) (subreg:SI (reg:V4DI <6>) 0)))
      (cinsn 10 (set (mem/c:SI (reg/f:DI <2>) [0  S4 A32]) (reg:SI <7>)))
      (edge-to exit (flags "FALLTHRU"))
    ) ;; block 2
  ) ;; insn-chain
) ;; function
}
