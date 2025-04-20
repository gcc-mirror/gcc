/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-additional-options "-O2 -march=x86-64-v3" } */

typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));
typedef int v2di __attribute__((vector_size(16)));

v4si __RTL (startwith ("vregs1")) foo1 (void)
{
(function "foo1"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:V4SI <1>)
		    (mem:V4SI (reg:SI di) [0 ptr S128 A128])))
      (cinsn 4 (set (reg:V4SI <2>)
		    (eq:V4SI (reg:V4SI <1>)
			     (mem:V4SI (reg:SI di) [0 ptr S128 A128]))))
      (cinsn 5 (set (reg:V4SI <3>) (reg:V4SI <2>)))
      (cinsn 6 (set (reg:V4SI xmm0) (reg:V4SI <3>)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
 (crtl (return_rtx (reg/i:V4SI xmm0)))
)
}

v8si __RTL (startwith ("vregs1")) foo2 (void)
{
(function "foo2"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:V8SI <1>)
		    (mem:V8SI (reg:SI di) [0 ptr S256 A256])))
      (cinsn 4 (set (reg:V8SI <2>)
		    (eq:V8SI (mem:V8SI (reg:SI di) [0 ptr S256 A256])
			     (reg:V8SI <1>))))
      (cinsn 5 (set (reg:V8SI <3>) (reg:V8SI <2>)))
      (cinsn 6 (set (reg:V8SI xmm0) (reg:V8SI <3>)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
 (crtl (return_rtx (reg/i:V8SI xmm0)))
)
}

v2di __RTL (startwith ("vregs1")) foo3 (void)
{
(function "foo3"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:V2DI <1>)
		    (mem:V2DI (reg:SI di) [0 ptr S128 A128])))
      (cinsn 4 (set (reg:V2DI <2>)
		    (eq:V2DI (reg:V2DI <1>)
			     (mem:V2DI (reg:SI di) [0 ptr S128 A128]))))
      (cinsn 5 (set (reg:V2DI <3>) (reg:V2DI <2>)))
      (cinsn 6 (set (reg:V2DI xmm0) (reg:V2DI <3>)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
 (crtl (return_rtx (reg/i:V2DI xmm0)))
)
}

/* { dg-final { scan-assembler-times "vpcmpeq" 3 } } */
