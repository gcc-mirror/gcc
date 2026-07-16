/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xiangshan-nanhu -fdump-rtl-sched2" } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-final { scan-rtl-dump {RISCV_FUSE_ZEXTW\M} "sched2" } } */

/*
**foo:
**	slli	a0,a0,32
**	srli	a0,a0,32
**	ret
*/
unsigned long foo(unsigned long x) {
  return (unsigned long)(unsigned int)x;
}
