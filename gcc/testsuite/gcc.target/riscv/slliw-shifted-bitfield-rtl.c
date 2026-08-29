/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-Os" "-Og" "-Oz" "-flto" } } */
/* { dg-options "-O2 -march=rv64gc_zicond -mabi=lp64d" } */

/* This RTL is roughly equivalent to:

     slli   a5,a0,40
     srai   a0,a5,44
     andi   a0,a0,-2
     slli   a0,a0,12

   which means a0[4..23] (sign_extract 20 bits from bit 4). It should
   not be rewritten into:

     andi   a0,a0,-2
     slliw  a0,a0,12

   because that would ignore the sign_extract start bit.  For example,
   x = 2 should produce 0 because bit 1 is outside a0[4..23], but the
   rewritten sequence would produce 0x2000.  */
unsigned long __RTL (startwith ("combine"))
foo_pos (unsigned long x)
{
(function "foo_pos"
  (param "x"
    (DECL_RTL (reg/v:DI <1> [ x ]))
    (DECL_RTL_INCOMING (reg:DI a0 [ x ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v:DI <1> [ x ])
                    (reg:DI a0 [ x ])))
      (cinsn 3 (set (reg:DI <2>)
                    (sign_extract:DI (reg/v:DI <1> [ x ])
                      (const_int 20)
                      (const_int 4))))
      (cinsn 4 (set (reg:DI <3>)
                    (ashift:DI (reg:DI <2>)
                      (const_int 12))))
      (cinsn 5 (set (reg:DI <0> [ <retval> ])
                    (and:DI (reg:DI <3>)
                      (const_int -8192))))
      (cinsn 6 (set (reg/i:DI a0)
                    (reg:DI <0> [ <retval> ])))
      (cinsn 7 (use (reg/i:DI a0)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
  (crtl
    (return_rtx
      (reg/i:DI a0)
    )
  )
)
}

/* This RTL is roughly equivalent to:

     slli   a5,a0,61
     srai   a0,a5,61
     andi   a0,a0,14
     slli   a0,a0,29

   The outer mask clears high 32 bits that SLLIW would instead fill
   from bit 31.  The shifted-bitfield split must not rewrite it as
   the following sequence:

     andi   a0,a0,-2
     slliw  a0,a0,29

   because that would sign-extend the final 32-bit result.  For example,
   x = 4 should produce 0x180000000, but the rewritten sequence would
   produce 0xffffffff80000000.  */
unsigned long __RTL (startwith ("combine"))
foo_mask (unsigned long x)
{
(function "foo_mask"
  (param "x"
    (DECL_RTL (reg/v:DI <1> [ x ]))
    (DECL_RTL_INCOMING (reg:DI a0 [ x ])))
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cinsn 2 (set (reg/v:DI <1> [ x ])
                    (reg:DI a0 [ x ])))
      (cinsn 3 (set (reg:DI <2>)
                    (ashift:DI (reg/v:DI <1> [ x ])
                      (const_int 61))))
      (cinsn 4 (set (reg:DI <3>)
                    (ashiftrt:DI (reg:DI <2>)
                      (const_int 62))))
      (cinsn 5 (set (reg:DI <4>)
                    (ashift:DI (reg:DI <3>)
                      (const_int 30))))
      (cinsn 6 (set (reg:DI <0> [ <retval> ])
                    (and:DI (reg:DI <4>)
                      (const_int 7516192768))))
      (cinsn 7 (set (reg/i:DI a0)
                    (reg:DI <0> [ <retval> ])))
      (cinsn 8 (use (reg/i:DI a0)))
      (edge-to exit (flags "FALLTHRU"))
    )
  )
  (crtl
    (return_rtx
      (reg/i:DI a0)
    )
  )
)
}

/* { dg-final { scan-assembler-not "slliw\t" } } */
