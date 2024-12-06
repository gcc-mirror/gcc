/* Document what we do for '__builtin_stack_save()', '__builtin_stack_restore()'.  */

/* { dg-do assemble } */
/* { dg-options {-O3 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void *p;

void f(void)
{
  // 0xdeadbeef
  p = __builtin_stack_save();
  asm volatile ("" : : : "memory");
  // no-op
  __builtin_stack_restore(p);
  asm volatile ("" : : : "memory");
}
/*
** f:
** \.visible \.func f
** {
** 	\.reg\.u64 (%r[0-9]+);
** 		mov\.u64	\1, 3735928559;
** 		st\.global\.u64	\[p\], \1;
** 	ret;
*/
