/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2" } */
/* { dg-additional-options "-fschedule-fusion -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Start before sched_fusion and disable sched2 to isolate the priority
   hook.  */

/*
**sched_fusion_load_pair:
**	...
**	ld	[a-z][0-9]+,0\(a0\)
**	ld	[a-z][0-9]+,8\(a0\)
**	...
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_load_pair (void)
{
(function "sched_fusion_load_pair"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (mem:DI (reg:DI a0) [0 S8 A64])))
      (cinsn 4 (set (reg:DI a3)
                    (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (reg:DI a2)
                    (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 8)) [0 S8 A64])))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 10)
  ) ;; insn-chain
  (crtl
    (return_rtx
      (reg/i:DI a0)
    ) ;; return_rtx
  ) ;; crtl
) ;; function "sched_fusion_load_pair"
}

/* Unaligned offsets must not receive memory-pair priorities.  */
/*
**sched_fusion_unaligned_loads:
**	...
**	ld	[a-z][0-9]+,4\(a0\)
**	add	[a-z][0-9]+,[a-z][0-9]+,[a-z][0-9]+
**	ld	[a-z][0-9]+,12\(a0\)
**	...
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_unaligned_loads (void)
{
(function "sched_fusion_unaligned_loads"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 4)) [0 S8 A32])))
      (cinsn 4 (set (reg:DI a3)
                    (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (reg:DI a2)
                    (mem:DI
                      (plus:DI (reg:DI a0)
                               (const_int 12)) [0 S8 A32])))
      (cinsn 6 (use (reg:DI a1)))
      (cinsn 7 (use (reg:DI a2)))
      (cinsn 8 (use (reg:DI a3)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    ) ;; block 2
    (cbarrier 10)
  ) ;; insn-chain
  (crtl
    (return_rtx (reg/i:DI a0))
  ) ;; crtl
) ;; function "sched_fusion_unaligned_loads"
}

/* Group initially decreasing, separated accesses in increasing order.  */
/*
**sched_fusion_reversed_input:
**	add	a3,a4,a5
**	ld	a2,0\(a0\)
**	ld	a1,8\(a0\)
**	add	a0,a1,a2
**	ret
*/
long __RTL (startwith ("compgotos"))
sched_fusion_reversed_input (void)
{
(function "sched_fusion_reversed_input"
  (insn-chain
    (block 2
      (edge-from entry (flags "FALLTHRU"))
      (cnote 1 [bb 2] NOTE_INSN_BASIC_BLOCK)
      (cnote 2 NOTE_INSN_FUNCTION_BEG)
      (cinsn 3 (set (reg:DI a1)
                    (mem:DI (plus:DI (reg:DI a0) (const_int 8)) [0 S8 A64])))
      (cinsn 4 (set (reg:DI a3) (plus:DI (reg:DI a4) (reg:DI a5))))
      (cinsn 5 (set (reg:DI a2) (mem:DI (reg:DI a0) [0 S8 A64])))
      (cinsn 6 (set (reg:DI a0) (plus:DI (reg:DI a1) (reg:DI a2))))
      (cinsn 7 (use (reg:DI a3)))
      (cinsn 8 (use (reg/i:DI a0)))
      (cjump_insn 9 (simple_return))
      (edge-to exit)
    )
    (cbarrier 10)
  )
  (crtl (return_rtx (reg/i:DI a0)))
)
}
