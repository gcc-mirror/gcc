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

.Lgots = .-.LCTOC1
	.long	_GOT_START_			# start of .got section

.Lgote = .-.LCTOC1
	.long	_GOT_END_			# end of .got section

.Lgot2s = .-.LCTOC1
	.long	_GOT2_START_			# -mrelocatable GOT pointers start

.Lgot2e = .-.LCTOC1
	.long	_GOT2_END_			# -mrelocatable GOT pointers end

	.text
.Lptr:
	.long	.LCTOC1-.Laddr			# PC relative pointer to .got2
	.long	0x4000				# traceback table

__eabi:	mflr	0
	bl	.Laddr				# get current address
.Laddr:
	mflr	12				# real address of .Laddr
	lwz	11,(.Lptr-.Laddr)(12)		# linker generated address of .LCTOC1
	add	11,11,12			# correct to real pointer
	lwz	12,.Ltable(11)			# get linker's idea of where .Laddr is
	subf.	12,12,11			# calculate difference
	mtlr	0				# restore link register
	bc	4,2,.Lreloc			# skip if we need to relocate

# Only load up register 2 if there is a .got section

	lwz	3,.Lgots(11)			# start of .got section
	lwz	4,.Lgote(11)			# end of .got section
	cmpw	1,3,4				# .got section non-empty?
	bc	12,6,.Ldone

# Normal program, load up register 2

	lwz	2,.Lgot(11)			# normal GOT address
	b	__do_global_ctors		# do any C++ global contstructors (which returns to caller)

# We need to relocate the .got2 pointers.  Don't load register 2

.Lreloc:
	lwz	3,.Lgot2s(11)			# GOT pointers start
	lwz	4,.Lgot2e(11)			# GOT pointers end
	add	3,12,3				# adjust pointers
	add	4,12,4

	cmpw	1,3,4				# any pointers to adjust
	bc	12,6,.Ldone

.Lloop:
	lwz	11,0(3)				# next pointer
	add	11,11,12			# adjust
	stw	11,0(3)
	addi	3,3,4				# bump to next word
	cmpw	1,3,4				# more pointers to adjust?
	bc	4,6,.Lloop

# Done adjusting pointers, return

.Ldone:
	b	__do_global_ctors		# do any C++ global contstructors (which returns to caller)

# Routines for saving floating point registers, called by the compiler.
# Called with r11 pointing to the stack header word of the caller of the
# function, just beyond the end of the floating point save area.

	.globl	_savefpr_14_l
	.globl	_savefpr_15_l
	.globl	_savefpr_16_l
	.globl	_savefpr_17_l
	.globl	_savefpr_18_l
	.globl	_savefpr_19_l
	.globl	_savefpr_20_l
	.globl	_savefpr_21_l
	.globl	_savefpr_22_l
	.globl	_savefpr_23_l
	.globl	_savefpr_24_l
	.globl	_savefpr_25_l
	.globl	_savefpr_26_l
	.globl	_savefpr_27_l
	.globl	_savefpr_28_l
	.globl	_savefpr_29_l
	.globl	_savefpr_30_l
	.globl	_savefpr_31_l

		.long	0x00400000	# traceback tag
_savefpr_14_l:	stfd	14,-144(11)	# save fp registers
_savefpr_15_l:  stfd	15,-136(11)
_savefpr_16_l:  stfd	16,-128(11)
_savefpr_17_l:  stfd	17,-120(11)
_savefpr_18_l:  stfd	18,-112(11)
_savefpr_19_l:  stfd	19,-104(11)
_savefpr_20_l:  stfd	20,-96(11)
_savefpr_21_l:  stfd	21,-88(11)
_savefpr_22_l:  stfd	22,-80(11)
_savefpr_23_l:  stfd	23,-72(11)
_savefpr_24_l:  stfd	24,-64(11)
_savefpr_25_l:  stfd	25,-56(11)
_savefpr_26_l:  stfd	26,-48(11)
_savefpr_27_l:  stfd	27,-40(11)
_savefpr_28_l:  stfd	28,-32(11)
_savefpr_29_l:  stfd	29,-24(11)
_savefpr_30_l:  stfd	30,-16(11)
_savefpr_31_l:  stfd	31,-8(11)
		stw	0,4(11)		# save return address also
		blr


# Routines for restoring floating point registers, called by the compiler.
# Called with r11 pointing to the stack header word of the caller of the
# function, just beyond the end of the floating point save area.

	.globl	_restfpr_14_l
	.globl	_restfpr_15_l
	.globl	_restfpr_16_l
	.globl	_restfpr_17_l
	.globl	_restfpr_18_l
	.globl	_restfpr_19_l
	.globl	_restfpr_20_l
	.globl	_restfpr_21_l
	.globl	_restfpr_22_l
	.globl	_restfpr_23_l
	.globl	_restfpr_24_l
	.globl	_restfpr_25_l
	.globl	_restfpr_26_l
	.globl	_restfpr_27_l
	.globl	_restfpr_28_l
	.globl	_restfpr_29_l
	.globl	_restfpr_30_l
	.globl	_restfpr_31_l

		.long	0x00600000	# traceback tag
_restfpr_14_l:	lfd	14,-144(11)	# restore fp registers
_restfpr_15_l:  lfd	15,-136(11)
_restfpr_16_l:  lfd	16,-128(11)
_restfpr_17_l:  lfd	17,-120(11)
_restfpr_18_l:  lfd	18,-112(11)
_restfpr_19_l:  lfd	19,-104(11)
_restfpr_20_l:  lfd	20,-96(11)
_restfpr_21_l:  lfd	21,-88(11)
_restfpr_22_l:  lfd	22,-80(11)
_restfpr_23_l:  lfd	23,-72(11)
_restfpr_24_l:  lfd	24,-64(11)
_restfpr_25_l:  lfd	25,-56(11)
_restfpr_26_l:  lfd	26,-48(11)
_restfpr_27_l:  lfd	27,-40(11)
_restfpr_28_l:  lfd	28,-32(11)
_restfpr_29_l:  lfd	29,-24(11)
_restfpr_30_l:  lfd	30,-16(11)
_restfpr_31_l:  lwz	0,4(11)		# caller's caller address
		lfd	31,-8(11)
		mtlr	0
		mr	1,11
		blr
