/* This is an assembly language implementation of libgcc1.c for the sparc
   processor.

   These routines are derived from the Sparc Architecture Manual, version 8,
   slightly edited to match the desired calling convention, and also to
   optimize them for our purposes.  */

#ifdef L_mulsi3
.text
	.align 4
	.global .umul
	.proc 4
.umul:
	or	%o0, %o1, %o4	! logical or of multiplier and multiplicand
	mov	%o0, %y		! multiplier to Y register
	andncc	%o4, 0xfff, %o5	! mask out lower 12 bits
	be	mul_shortway	! can do it the short way
	andcc	%g0, %g0, %o4	! zero the partial product and clear NV cc
	!
	! long multiply
	!
	mulscc	%o4, %o1, %o4	! first iteration of 33
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4	! 32nd iteration
	mulscc	%o4, %g0, %o4	! last iteration only shifts
	! the upper 32 bits of product are wrong, but we do not care
	retl
	rd	%y, %o0
	!
	! short multiply
	!
mul_shortway:
	mulscc	%o4, %o1, %o4	! first iteration of 13
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4
	mulscc	%o4, %o1, %o4	! 12th iteration
	mulscc	%o4, %g0, %o4	! last iteration only shifts
	rd	%y, %o5
	sll	%o4, 12, %o4	! left shift partial product by 12 bits
	srl	%o5, 20, %o5	! right shift partial product by 20 bits
	retl
	or	%o5, %o4, %o0	! merge for true product
#endif

#ifdef L_divsi3
.text
	.align 4
	.global	.udiv
	.proc 4
.udiv:
	save	%sp, -64, %sp
	b	divide
	mov	0, %i2		! result always positive
	.global .div
	.proc 4
.div:
	save	%sp, -64, %sp
	orcc	%i1, %i0, %g0	! is either operand negative
	bge	divide		! if not, skip this junk
	xor	%i1, %i0, %i2	! record sign of result in sign of %i2
	tst	%i1
	bge	2f
	tst	%i0
	! %i1 < 0
	bge	divide
	neg	%i1
2:	! %i0 < 0
	neg	%i0
	!	FALL THROUGH
divide:
	! Compute size of quotient, scale comparand.
	orcc	%i1, %g0, %l1		! movcc %i1, %l1
	te	2			! if %i1 = 0
	mov	%i0, %i3
	mov	0, %i2
	sethi	%hi(1<<(32-2-1)), %l3
	cmp 	%i3, %l3
	blu	not_really_big
	mov	0, %l0
	!
	! Here, the %i0 is >= 2^(31-3) or so.  We must be careful here,
	! as our usual 3-at-a-shot divide step will cause overflow and havoc.
	! The total number of bits in the result here is 3*%l0+%l4, where
	! %l4 <= 3.
	! Compute %l0 in an unorthodox manner: know we need to Shift %l1 into
	! the top decade: so do not even bother to compare to %i3.
1:	cmp	%l1, %l3
	bgeu	3f
	mov	1, %l4
	sll	%l1, 3, %l1
	b	1b
	inc	%l0
	!
	! Now compute %l4
	!
2:	addcc	%l1, %l1, %l1
	bcc	not_too_big
	add	%l4, 1, %l4
	!
	! We are here if the %i1 overflowed when Shifting.
	! This means that %i3 has the high-order bit set.
	! Restore %l1 and subtract from %i3.
	sll	%l3, 2, %l3
	srl	%l1, 1, %l1
	add	%l1, %l3, %l1
	b	do_single_div
	dec	%l4
not_too_big:
3:	cmp	%l1, %i3
	blu	2b
	nop
	be	do_single_div
	nop
	! %l1 > %i3: went too far: back up 1 step
	! 	srl	%l1, 1, %l1
	!	dec	%l4
	! do single-bit divide steps
	!
	! We have to be careful here.  We know that %i3 >= %l1, so we can do the
	! first divide step without thinking.  BUT, the others are conditional,
	! and are only done if %i3 >= 0.  Because both %i3 and %l1 may have the
	! high-order bit set in the first step, just falling into the regular
	! division loop will mess up the first time around.
	! So we unroll slightly...
do_single_div:
	deccc	%l4
	bl	end_regular_divide
	nop
	sub	%i3, %l1, %i3
	mov	1, %i2
	b	end_single_divloop
	nop
single_divloop:
	sll	%i2, 1, %i2
	bl	1f
	srl	%l1, 1, %l1
	! %i3 >= 0
	sub	%i3, %l1, %i3
	b	2f
	inc	%i2
1:	! %i3 < 0
	add	%i3, %l1, %i3
	dec	%i2
end_single_divloop:
2:	deccc	%l4
	bge	single_divloop
	tst	%i3
	b	end_regular_divide
	nop
not_really_big:
1:	sll	%l1, 3, %l1
	cmp	%l1, %i3
	bleu	1b
	inccc	%l0
	be	got_result
	dec	%l0
do_regular_divide:
	! Do the main division iteration
	tst	%i3
	! Fall through into divide loop
divloop:
	sll	%i2, 3, %i2
	! depth 1, accumulated bits 0
	bl	L.1.8
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	! depth 2, accumulated bits 1
	bl	L.2.9
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	! depth 3, accumulated bits 3
	bl	L.3.11
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (3*2+1), %i2
L.3.11:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (3*2-1), %i2
L.2.9:	! remainder is negative
	addcc	%i3,%l1,%i3
	! depth 3, accumulated bits 1
	bl	L.3.9
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (1*2+1), %i2
L.3.9:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (1*2-1), %i2
L.1.8:	! remainder is negative
	addcc	%i3,%l1,%i3
	! depth 2, accumulated bits -1
	bl	L.2.7
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	! depth 3, accumulated bits -1
	bl	L.3.7
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-1*2+1), %i2
L.3.7:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-1*2-1), %i2
L.2.7:	! remainder is negative
	addcc	%i3,%l1,%i3
	! depth 3, accumulated bits -3
	bl	L.3.5
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-3*2+1), %i2
L.3.5:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-3*2-1), %i2
end_regular_divide:
9:	deccc	%l0
	bge	divloop
	tst	%i3
	bge	got_result
	nop
	! non-restoring fixup here
	dec	%i2
got_result:
	tst	%i2
	bge	1f
	restore
	! answer < 0
	retl		! leaf-routine return
	neg	%o2, %o0	! quotient <- -%i2
1:	retl		! leaf-routine return
	mov	%o2, %o0	! quotient <- %i2
#endif

#ifdef L_modsi3
.text
	.align 4
	.global	.urem
	.proc 4
.urem:
	save	%sp, -64, %sp
	b	divide
	mov	0, %i2		! result always positive
	.global .rem
	.proc 4
.rem:
	save	%sp, -64, %sp
	orcc	%i1, %i0, %g0	! is either operand negative
	bge	divide		! if not, skip this junk
	mov	%i0, %i2	! record sign of result in sign of %i2
	tst	%i1
	bge	2f
	tst	%i0
	! %i1 < 0
	bge	divide
	neg	%i1
2:	! %i0 < 0
	neg	%i0
	!	FALL THROUGH
divide:
	! Compute size of quotient, scale comparand.
	orcc	%i1, %g0, %l1		! movcc %i1, %l1
	te	2			! if %i1 = 0
	mov	%i0, %i3
	mov	0, %i2
	sethi	%hi(1<<(32-2-1)), %l3
	cmp 	%i3, %l3
	blu	not_really_big
	mov	0, %l0
	!
	! Here, the %i0 is >= 2^(31-3) or so.  We must be careful here,
	! as our usual 3-at-a-shot divide step will cause overflow and havoc.
	! The total number of bits in the result here is 3*%l0+%l4, where
	! %l4 <= 3.
	! Compute %l0 in an unorthodox manner: know we need to Shift %l1 into
	! the top decade: so do not even bother to compare to %i3.
1:	cmp	%l1, %l3
	bgeu	3f
	mov	1, %l4
	sll	%l1, 3, %l1
	b	1b
	inc	%l0
	!
	! Now compute %l4
	!
2:	addcc	%l1, %l1, %l1
	bcc	not_too_big
	add	%l4, 1, %l4
	!
	! We are here if the %i1 overflowed when Shifting.
	! This means that %i3 has the high-order bit set.
	! Restore %l1 and subtract from %i3.
	sll	%l3, 2, %l3
	srl	%l1, 1, %l1
	add	%l1, %l3, %l1
	b	do_single_div
	dec	%l4
not_too_big:
3:	cmp	%l1, %i3
	blu	2b
	nop
	be	do_single_div
	nop
	! %l1 > %i3: went too far: back up 1 step
	! 	srl	%l1, 1, %l1
	!	dec	%l4
	! do single-bit divide steps
	!
	! We have to be careful here.  We know that %i3 >= %l1, so we can do the
	! first divide step without thinking.  BUT, the others are conditional,
	! and are only done if %i3 >= 0.  Because both %i3 and %l1 may have the
	! high-order bit set in the first step, just falling into the regular
	! division loop will mess up the first time around.
	! So we unroll slightly...
do_single_div:
	deccc	%l4
	bl	end_regular_divide
	nop
	sub	%i3, %l1, %i3
	mov	1, %i2
	b	end_single_divloop
	nop
single_divloop:
	sll	%i2, 1, %i2
	bl	1f
	srl	%l1, 1, %l1
	! %i3 >= 0
	sub	%i3, %l1, %i3
	b	2f
	inc	%i2
1:	! %i3 < 0
	add	%i3, %l1, %i3
	dec	%i2
end_single_divloop:
2:	deccc	%l4
	bge	single_divloop
	tst	%i3
	b	end_regular_divide
	nop
not_really_big:
1:	sll	%l1, 3, %l1
	cmp	%l1, %i3
	bleu	1b
	inccc	%l0
	be	got_result
	dec	%l0
do_regular_divide:
	! Do the main division iteration
	tst	%i3
	! Fall through into divide loop
divloop:
	sll	%i2, 3, %i2
	! depth 1, accumulated bits 0
	bl	L.1.8
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	! depth 2, accumulated bits 1
	bl	L.2.9
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	! depth 3, accumulated bits 3
	bl	L.3.11
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (3*2+1), %i2
L.3.11:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (3*2-1), %i2
L.2.9:	! remainder is negative
	addcc	%i3,%l1,%i3
	! depth 3, accumulated bits 1
	bl	L.3.9
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (1*2+1), %i2
L.3.9:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (1*2-1), %i2
L.1.8:	! remainder is negative
	addcc	%i3,%l1,%i3
	! depth 2, accumulated bits -1
	bl	L.2.7
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	! depth 3, accumulated bits -1
	bl	L.3.7
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-1*2+1), %i2
L.3.7:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-1*2-1), %i2
L.2.7:	! remainder is negative
	addcc	%i3,%l1,%i3
	! depth 3, accumulated bits -3
	bl	L.3.5
	srl	%l1,1,%l1
	! remainder is positive
	subcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-3*2+1), %i2
L.3.5:	! remainder is negative
	addcc	%i3,%l1,%i3
	b	9f
	add	%i2, (-3*2-1), %i2
end_regular_divide:
9:	deccc	%l0
	bge	divloop
	tst	%i3
	bge	got_result
	nop
	! non-restoring fixup here
	add	%i3, %i1, %i3
got_result:
	tst	%i2
	bge	1f
	restore
	! answer < 0
	retl		! leaf-routine return
	neg	%o3, %o0	! remainder <- -%i3
1:	retl		! leaf-routine return
	mov	%o3, %o0	! remainder <- %i3
#endif


