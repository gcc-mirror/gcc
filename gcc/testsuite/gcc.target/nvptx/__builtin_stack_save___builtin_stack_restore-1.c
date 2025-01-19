/* Document what we do for '__builtin_stack_save()', '__builtin_stack_restore()'.  */

/* { dg-do assemble } */
/* { dg-options {-O3 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void *p;

void f(void)
{
  p = __builtin_stack_save();
  asm volatile ("" : : : "memory");
  __builtin_stack_restore(p);
  asm volatile ("" : : : "memory");
}
/*
** f:
** \.visible \.func f
** {
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 		stacksave\.u64	\1;
** 		st\.global\.u64	\[p\], \1;
** 		ld\.global\.u64	\2, \[p\];
** 		stackrestore\.u64	\2;
** 	ret;
*/
