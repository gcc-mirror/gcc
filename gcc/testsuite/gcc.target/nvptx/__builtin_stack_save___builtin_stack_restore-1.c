/* Document what we do for '__builtin_stack_save()', '__builtin_stack_restore()'.  */

/* { dg-do compile }
   TODO We can't 'assemble' this -- it's invalid PTX code.  */
/* { dg-options {-O3 -mno-soft-stack} } */
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
** 		st\.global\.u64	\[p\], %stack;
** 	ret;
*/

/* The concept of a '%stack' pointer doesn't apply like this for
   '-mno-soft-stack': PTX "native" stacks (TODO).

   { dg-final { scan-assembler-not {%stack} { xfail *-*-* } } } */

/* As these are an internal-use built-in function, we don't bother with
   emitting proper error diagnostics.  */
