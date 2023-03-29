/* { dg-do run } */
/* { dg-require-effective-target fstack_protector } */
/* { dg-options "-fstack-protector-all -O2" } */

extern volatile long *stack_chk_guard_ptr;

volatile long *
get_ptr (void)
{
  return stack_chk_guard_ptr;
}

void __attribute__ ((noipa))
f (void)
{
  volatile int x;
  x = 1;
  x += 1;
}

#define CHECK(REG) "\tcmp\tr0, " #REG "\n\tbeq\t1f\n"

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
"	.globl	main\n"
"	.type	main, %function\n"
"main:\n"
"	bl	get_ptr\n"
"	sub	sp, sp, #8\n"
"	str	r0, [sp]\n"
"	bl	f\n"
"	str	r0, [sp, #4]\n"
"	ldr     r0, [sp]\n"
"	ldr     r0, [r0]\n"
	CHECK (r1)
	CHECK (r2)
	CHECK (r3)
	CHECK (r4)
	CHECK (r5)
	CHECK (r6)
	CHECK (r7)
	CHECK (r8)
	CHECK (r9)
	CHECK (r10)
	CHECK (r11)
	CHECK (r12)
	CHECK (r14)
"	ldr	r1, [sp, #4]\n"
	CHECK (r1)
"	mov	r0, #0\n"
"	bl	exit\n"
"1:\n"
"	bl	abort\n"
"	.size	main, .-main"
);
