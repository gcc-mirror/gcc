;# New Alpha OpenVMS trampoline
;#
	.set noreorder
	.set volatile
	.set noat
	.file 1 "tramp.s"
.text
	.align 3
	.globl __tramp
	.ent __tramp
__tramp..en:

.link
	.align 3
__tramp:
	.pdesc __tramp..en,null
.text
	ldq $1,24($27)
	ldq $27,16($27)
	ldq $28,8($27)
	jmp $31,($28),0
	.end __tramp
