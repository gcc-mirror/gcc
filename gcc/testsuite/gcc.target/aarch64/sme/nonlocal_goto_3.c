/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

void run(void (*)());

/*
** foo:
**	...
**	smstart	sm
**	...
**	smstop	sm
**	bl	__clear_cache
**	smstart	sm
**	add	x0, .*
**	smstop	sm
**	bl	run
**	smstart	sm
**	mov	w0, 1
**	...
**	smstart	sm
**	mov	w0, 0
**	smstop	sm
**	...
*/
__arm_locally_streaming int
foo (int *ptr)
{
  __label__ failure;

  void bar () { *ptr += 1; goto failure; }
  run (bar);
  return 1;

failure:
  return 0;
}

// { dg-final { scan-assembler {\tstp\tx19, x20,} } }
// { dg-final { scan-assembler {\tstp\tx21, x22,} } }
// { dg-final { scan-assembler {\tstp\tx23, x24,} } }
// { dg-final { scan-assembler {\tstp\tx25, x26,} } }
// { dg-final { scan-assembler {\tstp\tx27, x28,} } }
// { dg-final { scan-assembler {\tstp\td8, d9,} } }
// { dg-final { scan-assembler {\tstp\td10, d11,} } }
// { dg-final { scan-assembler {\tstp\td12, d13,} } }
// { dg-final { scan-assembler {\tstp\td14, d15,} } }
