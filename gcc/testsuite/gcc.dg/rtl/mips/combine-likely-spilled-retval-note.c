/* { dg-do compile { target mips*-*-* } } */
/* { dg-options "-O2 -g -mips1 -mabi=32 -mfix-r4000 -fdump-rtl-combine-details" } */

/* Verify that a trailing debug instruction does not cause the scan to skip
   a note that it would retain without debug information.  */

long long __RTL (startwith ("combine"))
f (long long a, long long b)
{
(function "f"
  (param "a"
    (DECL_RTL (reg/v:DI <3> [ a ]))
    (DECL_RTL_INCOMING (reg:DI $4 [ a ])))
  (param "b"
    (DECL_RTL (reg/v:DI <4> [ b ]))
    (DECL_RTL_INCOMING (reg:DI $6 [ b ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 8 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 19 (set (reg:SI <0> [ a+4 ])
                     (reg:SI $5 [ a+4 ])))
      (cinsn 21 (set (reg:SI <1> [ b+4 ])
                     (reg:SI $7 [ b+4 ])))
      (cnote 7 NOTE_INSN_FUNCTION_BEG)
      (cinsn 10 (set (reg:DI <2>)
                     (mult:DI
                       (sign_extend:DI (reg:SI <0> [ a+4 ]))
                       (sign_extend:DI (reg:SI <1> [ b+4 ])))))
      (cinsn 15 (set (reg/i:DI $2)
                     (reg:DI <2>)))
      (cinsn 16 (use (reg/i:DI $2)))
      (cnote 18 NOTE_INSN_DELETED)
      (cdebug_insn 17 (debug_marker))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
  (crtl
    (return_rtx (reg/i:DI $2))
  )
)
}

/* The combine cost dump prints the BEGIN_STMT marker.  Check it so that the
   other scans cannot pass because debug information was discarded.  */
/* { dg-final { scan-rtl-dump "debug begin stmt marker" "combine" } } */
/* { dg-final { scan-rtl-dump "allowing combination of insns 10 and 15" "combine" } } */
