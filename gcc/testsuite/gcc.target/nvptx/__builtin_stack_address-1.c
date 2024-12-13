/* Document what we do for '__builtin_stack_address()'.  */

/* { dg-do compile }
   TODO We can't 'assemble' this -- it's invalid PTX code.  */
/* { dg-options -O3 } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void f(void)
{
  void *p;
  p = __builtin_stack_address();
  sink(p);
}
/*
** f:
** \.visible \.func f
** {
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], %stack;
** 		call sink, \(%out_arg1\);
** 	}
** 	ret;
*/

/* The concept of a '%stack' pointer doesn't apply like this for
   '-mno-soft-stack': PTX "native" stacks (TODO), and for '-msoft-stack' in
   this form also constitutes invalid PTX code (TODO).

   { dg-final { scan-assembler-not {%stack} { xfail *-*-* } } } */

/* As this is an internal-use built-in function, we don't bother with
   emitting proper error diagnostics.  */
