# Allocate stack for NT, inserting stack probes every 4k pages

	.file	"ntstack.asm"

#	Setup MS Structured-Exception-Handling
	.pdata
	.align 2
	.ualong ..__allocate_stack,__allocate_stack.e,0,0,__allocate_stack.b

#	Switch to the relocation section
	.reldata
	.globl __allocate_stack
	.globl ..__allocate_stack
__allocate_stack:
	.ualong ..__allocate_stack,.toc

	.text
	.align 2
..__allocate_stack:
	.function	..__allocate_stack
__allocate_stack.b:
	addi	3,3,15			# round up to 16 byte alignment
	lwz	0,0(1)			# old stack link
	rlwinm	3,3,0,0,28
	srawi.	4,3,12			# get # of pages to check
	neg	3,3			# negate so we can use stwux
	bgt-	0,.Lcheck
	stwux	0,1,3			# small request, just decrement and return
	blr

.Lcheck:
	mtctr	4			# number of pages to check
	mr	5,1			# tmp pointer
.Lloop:
	lwzu	6,-4096(5)		# touch the page
	bdnz+	.Lloop			# and loop back

	stwux	0,1,3			# update stack pointer
	blr

__allocate_stack.e:
FE_MOT_RESVD..__allocate_stack:
