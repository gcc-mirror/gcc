/* Document what we run into for '__builtin_alloca(0)'.  */

/* { dg-do compile }
   TODO We can't 'assemble' this -- it's invalid PTX code.  */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

/* See 'gcc.c-torture/execute/pr36321.c', '-O0'.  */

void sink(void *);

void f(void)
{
  sink(__builtin_alloca(0));
}
/*
** f:
** .visible .func f
** {
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], %stack;
** 		call sink, \(%out_arg1\);
** 	}
** 	ret;
*/

/* '%stack' is 'VIRTUAL_STACK_DYNAMIC_REGNUM', 'virtual_stack_dynamic_rtx'.
   For '__builtin_alloca (0)', we get to
   'gcc/explow.cc:allocate_dynamic_stack_space', where 'addr' gets set to
   'virtual_stack_dynamic_rtx', 'size == const0_rtx', therefore 'return addr;',
   which gets us '%stack' -- undefined (TODO).

   { dg-final { scan-assembler-not {%stack} { xfail *-*-* } } } */
