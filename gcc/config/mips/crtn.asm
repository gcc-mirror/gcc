/* 4 slots for argument spill area.  1 for cpreturn, 1 for stack.
   Return spill offset of 40 and 20.  Aligned to 16 bytes for n32.  */

	.section .init,"ax",@progbits
#ifdef	__mips16
/* The mips16 uses $7 for a return address. We use that here too.  */
	lw	$7,20($sp)
	addu	$sp,$sp,32

	j	$7
#else
#ifdef __mips64
	ld      $31,40($sp)
	daddu	$sp,$sp,48
#else
	lw	$31,20($sp)
	addu	$sp,$sp,32
#endif
	j	$31

#endif

	.section .fini,"ax",@progbits
#ifdef	__mips16
/* The mips16 uses $7 for a return address. We use that here too.  */
	lw	$7,20($sp)
	addu	$sp,$sp,32

	j	$7
#else
#ifdef	__mips64
	ld	$31,40($sp)
	daddu	$sp,$sp,48
#else
	lw	$31,20($sp)
	addu	$sp,$sp,32
#endif
	j	$31
#endif
