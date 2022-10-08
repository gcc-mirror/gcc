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

asm (
"	.data\n"
"	.align	3\n"
"	.globl	stack_chk_guard_ptr\n"
"stack_chk_guard_ptr:\n"
"	.word	__stack_chk_guard\n"
"	.weak	__stack_chk_guard\n"
"__stack_chk_guard:\n"
"	.word	0xdead4321\n"
"	.text\n"
"	.type	__stack_chk_fail, %function\n"
"__stack_chk_fail:\n"
"	movs	r0, #0\n"
"	bl	exit\n"
"	.size	__stack_chk_fail, .-__stack_chk_fail"
);

int
main (void)
{
  f ();
  __builtin_abort ();
}
