	.abicalls
	.set	noreorder
	.set	nomacro

	.section .init,0x1,0x6,4,4
	jr	$31
	nop

	.globl	__gcc_init
__gcc_init:
#if _MIPS_SIM == _ABIO32
	addiu	$sp,$sp,-16
	sw	$31,0($sp)
#else
	daddiu	$sp,$sp,-16
	sd	$31,0($sp)
	sd	$28,8($sp)
#endif

	.section .fini,0x1,0x6,4,4
	jr	$31
	nop

	.globl	__gcc_fini
__gcc_fini:
#if _MIPS_SIM == _ABIO32
	addiu	$sp,$sp,-16
	sw	$31,0($sp)
#else
	daddiu	$sp,$sp,-16
	sd	$31,0($sp)
	sd	$28,8($sp)
#endif
