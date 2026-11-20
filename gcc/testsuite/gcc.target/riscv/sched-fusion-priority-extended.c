/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "RTL input" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fschedule-fusion -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Exercise signed loads, stores, zero stores and offset ordering on either
   side of zero.  Unsigned word loads are not C9501 pair candidates.  */

/*
**sched_fusion_signed_loads:
**	add	a3,a4,a5
**	lw	a1,-8\(a0\)
**	lw	a2,-4\(a0\)
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_signed_loads (void)
{
(function "sched_fusion_signed_loads"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (sign_extend:DI
                      (mem:SI (plus:DI (reg:DI a0) (const_int -8))
                      [0 S4 A32]))))
      (cinsn 4 (set (reg:DI a3) (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (reg:DI a2)
                    (sign_extend:DI
                      (mem:SI (plus:DI (reg:DI a0) (const_int -4))
                      [0 S4 A32]))))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    )
    (cbarrier 10)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}

/*
**sched_fusion_negative_loads:
**	add	a3,a4,a5
**	ld	a1,-16\(a0\)
**	ld	a2,-8\(a0\)
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_negative_loads (void)
{
(function "sched_fusion_negative_loads"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (mem:DI (plus:DI (reg:DI a0) (const_int -16))
                      [0 S8 A64])))
      (cinsn 4 (set (reg:DI a3) (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (reg:DI a2)
                    (mem:DI (plus:DI (reg:DI a0) (const_int -8))
                      [0 S8 A64])))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    )
    (cbarrier 10)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}

/*
**sched_fusion_stores:
**	add	a3,a4,a5
**	sd	a1,0\(a0\)
**	sd	a2,8\(a0\)
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_stores (void)
{
(function "sched_fusion_stores"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:DI (reg:DI a0)
                      [0 S8 A64])
                    (reg:DI a1)))
      (cinsn 4 (set (reg:DI a3) (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (mem:DI (plus:DI (reg:DI a0) (const_int 8))
                      [0 S8 A64])
                    (reg:DI a2)))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    )
    (cbarrier 10)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}

/*
**sched_fusion_cross_zero_stores:
**	add	a3,a4,a5
**	sw	a1,-4\(a0\)
**	sw	a2,0\(a0\)
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_cross_zero_stores (void)
{
(function "sched_fusion_cross_zero_stores"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:SI (plus:DI (reg:DI a0) (const_int -4))
                      [0 S4 A32])
                    (reg:SI a1)))
      (cinsn 4 (set (reg:DI a3) (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (mem:SI (reg:DI a0)
                      [0 S4 A32])
                    (reg:SI a2)))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    )
    (cbarrier 10)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}

/*
**sched_fusion_zero_stores:
**	add	a3,a4,a5
**	sd	zero,16\(a0\)
**	sd	zero,24\(a0\)
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_zero_stores (void)
{
(function "sched_fusion_zero_stores"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (mem:DI (plus:DI (reg:DI a0) (const_int 16))
                      [0 S8 A64])
                    (const_int 0)))
      (cinsn 4 (set (reg:DI a3) (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (mem:DI (plus:DI (reg:DI a0) (const_int 24))
                      [0 S8 A64])
                    (const_int 0)))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    )
    (cbarrier 10)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}

/*
**sched_fusion_unsigned_loads:
**	lwu	a1,0\(a0\)
**	add	a3,a4,a5
**	lwu	a2,4\(a0\)
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_unsigned_loads (void)
{
(function "sched_fusion_unsigned_loads"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (zero_extend:DI
                      (mem:SI (reg:DI a0)
                      [0 S4 A32]))))
      (cinsn 4 (set (reg:DI a3) (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (reg:DI a2)
                    (zero_extend:DI
                      (mem:SI (plus:DI (reg:DI a0) (const_int 4))
                      [0 S4 A32]))))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    )
    (cbarrier 10)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}
