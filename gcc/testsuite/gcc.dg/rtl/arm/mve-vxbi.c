/* { dg-do compile { target arm*-*-* } } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2" } */

void __RTL (startwith ("ira")) foo (void *ptr)
{
  (function "foo"
   (param "ptr"
    (DECL_RTL (reg/v:SI <0> [ ptr ]))
    (DECL_RTL_INCOMING (reg:SI r0 [ ptr ]))
    ) ;; param "n"
   (insn-chain
    (block 2
     (edge-from entry (flags "FALLTHRU"))
     (cnote 5 [bb 2] NOTE_INSN_BASIC_BLOCK)
     (insn 7 (set (reg:V4BI <1>)
	      (const_vector:V4BI [(const_int 1)
				  (const_int 0)
				  (const_int 0)
				  (const_int 1)])) (nil))
     (insn 8 (set (mem:V4BI (reg:SI <0>) [1 ptr+0 S2 A16]) (reg:V4BI <1>)))
     (edge-to exit (flags "FALLTHRU"))
     ) ;; block 2
    ) ;; insn-chain
   ) ;; function
}

void __RTL (startwith ("ira")) foo2 (void *ptr)
{
  (function "foo"
   (param "ptr"
    (DECL_RTL (reg/v:SI <0> [ ptr ]))
    (DECL_RTL_INCOMING (reg:SI r0 [ ptr ]))
    ) ;; param "n"
   (insn-chain
    (block 2
     (edge-from entry (flags "FALLTHRU"))
     (cnote 5 [bb 2] NOTE_INSN_BASIC_BLOCK)
     (insn 7 (set (reg:V8BI <1>)
	      (const_vector:V8BI [(const_int 1)
				  (const_int 0)
				  (const_int 1)
				  (const_int 1)
				  (const_int 1)
				  (const_int 1)
				  (const_int 0)
				  (const_int 1)])) (nil))
     (insn 8 (set (mem:V8BI (reg:SI <0>) [1 ptr+0 S2 A16]) (reg:V8BI <1>)))
     (edge-to exit (flags "FALLTHRU"))
     ) ;; block 2
    ) ;; insn-chain
   ) ;; function
}

void __RTL (startwith ("ira")) foo3 (void *ptr)
{
  (function "foo"
   (param "ptr"
    (DECL_RTL (reg/v:SI <0> [ ptr ]))
    (DECL_RTL_INCOMING (reg:SI r0 [ ptr ]))
    ) ;; param "n"
   (insn-chain
    (block 2
     (edge-from entry (flags "FALLTHRU"))
     (cnote 5 [bb 2] NOTE_INSN_BASIC_BLOCK)
     (insn 7 (set (reg:V16BI <1>)
	      (const_vector:V16BI [(const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)
				  (const_int 0)])) (nil))
     (insn 8 (set (mem:V16BI (reg:SI <0>) [1 ptr+0 S2 A16]) (reg:V16BI <1>)))
     (edge-to exit (flags "FALLTHRU"))
     ) ;; block 2
    ) ;; insn-chain
   ) ;; function
}
