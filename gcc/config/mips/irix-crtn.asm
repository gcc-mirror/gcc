	.abicalls
	.set	noreorder
	.set	nomacro

	.section .init,0x1,0x6,4,4
#if _MIPS_SIM == _ABIO32
	lw	$31,0($sp)
	jr	$31
	addiu	$sp,$sp,16
#else
	ld	$31,0($sp)
	ld	$28,8($sp)
	jr	$31
	daddiu	$sp,$sp,16
#endif

	.section .fini,0x1,0x6,4,4
#if _MIPS_SIM == _ABIO32
	lw	$31,0($sp)
	jr	$31
	addiu	$sp,$sp,16
#else
	ld	$31,0($sp)
	ld	$28,8($sp)
	jr	$31
	daddiu	$sp,$sp,16
#endif
