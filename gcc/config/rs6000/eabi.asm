# File to either setup register 2 to point to the GOT, or to adjust the
# pointers in the .got2 section to point to their new addresses.

	.file	"eabi.asm"
	.section ".text"
	.globl	 __eabi

	 .section ".got2","aw"
.LCTOC1 = . # +32768

# Table of addresses
.Ltable = .-.LCTOC1
	.long	.LCTOC1				# address we are really at

.Lgot = .-.LCTOC1
	.long	_GLOBAL_OFFSET_TABLE_		# normal GOT address

.Lgot2s = .-.LCTOC1
	.long	_GOT2_START_			# -mrelocatable GOT pointers start

.Lgot2e = .-.LCTOC1
	.long	_GOT2_END_			# -mrelocatable GOT pointers end

	.text
.Lptr:	.long	.LCTOC1-.Laddr			# PC relative pointer to .got2
	.long	0x4000				# traceback table

__eabi:	mflr	0
	bl	.Laddr				# get current address
.Laddr:	mflr	12				# real address of .Laddr
	lwz	11,(.Lptr-.Laddr)(12)		# linker generated address of .LCTOC1
	add	11,11,12			# correct to real pointer
	lwz	12,.Ltable(11)			# get linker's idea of where .Laddr is
	subf.	12,12,11			# calculate difference
	bc	4,2,.Lreloc			# skip if we need to relocate

# Normal program, load up register 2

	mtlr	0				# restore link register
	lwz	2,.Lgot(11)			# normal GOT address
	blr

# We need to relocate the .got2 pointers.  Don't load register 2

.Lreloc:
	stwu	30,-4(1)
	stwu	31,-4(1)
	lwz	30,.Lgot2s(11)			# GOT pointers start
	lwz	31,.Lgot2e(11)			# GOT pointers end
	add	30,12,30			# adjust pointers
	add	31,12,31

	cmpw	1,30,31				# any pointers to adjust
	bc	12,6,.Ldone

.Lloop:	lwz	11,0(30)			# next pointer
	add	11,11,12			# adjust
	stw	11,0(30)
	addi	30,30,4				# bump to next word
	cmpw	1,30,31				# more pointers to adjust?
	bc	4,6,.Lloop

# Done adjusting pointers, return

.Ldone:
	mtlr	0				# restore link register
	lwz	31,0(1)
	lwz	30,4(1)				# restore regs
	addic	1,1,8				# pop stack
	blr
