/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O -fdisable-rtl-postreload -fpeephole2 -fno-schedule-fusion" } */

extern int data[];

void __RTL (startwith ("ira")) foo (void *ptr)
{
  (function "foo"
    (param "ptr"
      (DECL_RTL (reg/v:DI <0> [ ptr ]))
      (DECL_RTL_INCOMING (reg/v:DI x0 [ ptr ]))
    ) ;; param "ptr"
    (insn-chain
      (block 2
	(edge-from entry (flags "FALLTHRU"))
	(cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
	(insn 4 (set (reg:DI <0>) (reg:DI x0)))
	(insn 5 (set (reg:DI <1>)
		     (plus:DI (reg:DI <0>) (const_int 768))))
	(insn 6 (set (mem:SI (plus:DI (reg:DI <0>)
				      (const_int 508)) [1 &data+508 S4 A4])
		     (const_int 0)))
	(insn 7 (set (mem:SI (plus:DI (reg:DI <1>)
				      (const_int -256)) [1 &data+512 S4 A4])
		     (const_int 0)))
	(edge-to exit (flags "FALLTHRU"))
      ) ;; block 2
    ) ;; insn-chain
  ) ;; function
}

void __RTL (startwith ("ira")) bar (void *ptr)
{
  (function "bar"
    (param "ptr"
      (DECL_RTL (reg/v:DI <0> [ ptr ]))
      (DECL_RTL_INCOMING (reg/v:DI x0 [ ptr ]))
    ) ;; param "ptr"
    (insn-chain
      (block 2
	(edge-from entry (flags "FALLTHRU"))
	(cnote 3 [bb 2] NOTE_INSN_BASIC_BLOCK)
	(insn 4 (set (reg:DI <0>) (reg:DI x0)))
	(insn 5 (set (reg:DI <1>)
		     (plus:DI (reg:DI <0>) (const_int 768))))
	(insn 6 (set (mem:SI (plus:DI (reg:DI <1>)
				      (const_int -256)) [1 &data+512 S4 A4])
		     (const_int 0)))
	(insn 7 (set (mem:SI (plus:DI (reg:DI <0>)
				      (const_int 508)) [1 &data+508 S4 A4])
		     (const_int 0)))
	(edge-to exit (flags "FALLTHRU"))
      ) ;; block 2
    ) ;; insn-chain
  ) ;; function
}
