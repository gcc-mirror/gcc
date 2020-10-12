/* { dg-do run } */
/* { dg-require-effective-target fstack_protector } */
/* { dg-options "-fstack-protector-all -O2" } */

extern volatile long *stack_chk_guard_ptr;

void __attribute__ ((noipa))
f (void)
{
  volatile int x;
  /* Munging the contents of __stack_chk_guard should trigger a
     stack-smashing failure for this function.  */
  *stack_chk_guard_ptr += 1;
}

#define CHECK(REG) "\tcmp\tx0, " #REG "\n\tbeq\t1f\n"

asm (
"	.pushsection .data\n"
"	.align	3\n"
"stack_chk_guard_ptr:\n"
#if __ILP32__
"	.word	__stack_chk_guard\n"
#else
"	.xword	__stack_chk_guard\n"
#endif
"	.weak	__stack_chk_guard\n"
"__stack_chk_guard:\n"
"	.word	0xdead4321\n"
"	.word	0xbeef8765\n"
"	.text\n"
"	.type	__stack_chk_fail, %function\n"
"__stack_chk_fail:\n"
"	mov	x0, #0\n"
"	b	exit\n"
"	.size	__stack_chk_fail, .-__stack_chk_fail\n"
"	.popsection"
);

int
main (void)
{
  f ();
  __builtin_abort ();
}
