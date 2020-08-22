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

#define CHECK(REG) "\tcmp\tx0, " #REG "\n\tbeq\t1f\n"

asm (
"	.pushsection .data\n"
"	.align	3\n"
"	.globl	stack_chk_guard_ptr\n"
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
"	.globl	main\n"
"	.type	main, %function\n"
"main:\n"
"	bl	get_ptr\n"
"	str	x0, [sp, #-16]!\n"
"	bl	f\n"
"	str	x0, [sp, #8]\n"
"	ldr	x0, [sp]\n"
#if __ILP32__
"	ldr     w0, [x0]\n"
#else
"	ldr     x0, [x0]\n"
#endif
	CHECK (x1)
	CHECK (x2)
	CHECK (x3)
	CHECK (x4)
	CHECK (x5)
	CHECK (x6)
	CHECK (x7)
	CHECK (x8)
	CHECK (x9)
	CHECK (x10)
	CHECK (x11)
	CHECK (x12)
	CHECK (x13)
	CHECK (x14)
	CHECK (x15)
	CHECK (x16)
	CHECK (x17)
	CHECK (x18)
	CHECK (x19)
	CHECK (x20)
	CHECK (x21)
	CHECK (x22)
	CHECK (x23)
	CHECK (x24)
	CHECK (x25)
	CHECK (x26)
	CHECK (x27)
	CHECK (x28)
	CHECK (x29)
	CHECK (x30)
"	ldr	x1, [sp]\n"
	CHECK (x1)
"	mov	x0, #0\n"
"	b	exit\n"
"1:\n"
"	b	abort\n"
"	.size	main, .-main\n"
"	.popsection"
);
