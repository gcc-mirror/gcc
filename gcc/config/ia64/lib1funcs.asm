#ifdef L__divdf3
// Compute a 64-bit IEEE double quotient.
//
// From the Intel IA-64 Optimization Guide, choose the minimum latency
// alternative.
//
// farg0 holds the dividend.  farg1 holds the divisor.

	.text
	.align 16
	.global __divdf3
	.proc __divdf3
__divdf3:
	frcpa f10, p6 = farg0, farg1
	;;
(p6)	fma.s1 f11 = farg0, f10, f0
(p6)	fnma.s1 f12 = farg1, f10, f1
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f13 = f12, f12, f0
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fma.s1 f11 = f13, f11, f11
(p6)	fma.s1 f12 = f13, f13, f0
(p6)	fma.s1 f10 = f13, f10, f10
	;;
(p6)	fma.d.s1 f11 = f12, f11, f11
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fnma.d.s1 f8 = farg1, f11, farg0
	;;
(p6)	fma.d f10 = f8, f10, f11
	;;
	mov fret0 = f10
	br.ret.sptk rp
	;;
	.endp __divdf3
#endif

#ifdef L__divsf3
// Compute a 32-bit IEEE float quotient.
//
// From the Intel IA-64 Optimization Guide, choose the minimum latency
// alternative.
//
// farg0 holds the dividend.  farg1 holds the divisor.

	.text
	.align 16
	.global __divsf3
	.proc __divsf3
__divsf3:
	frcpa f10, p6 = farg0, farg1
	;;
(p6)	fma.s1 f8 = farg0, f10, f0
(p6)	fnma.s1 f9 = farg1, f10, f1
	;;
(p6)	fma.s1 f8 = f9, f8, f8
(p6)	fma.s1 f9 = f9, f9, f0
	;;
(p6)	fma.s1 f8 = f9, f8, f8
(p6)	fma.s1 f9 = f9, f9, f0
	;;
(p6)	fma.d.s1 f8 = f9, f8, f8
	;;
(p6)	fma.s f10 = f8, f1, f0
	;;
	mov fret0 = f10
	br.ret.sptk rp
	;;
	.endp __divsf3
#endif

#ifdef L__divdi3
// Compute a 64-bit integer quotient.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 3 iterations
// to get more than the 64 bits of precision that we need for DImode.
//
// Must use max precision for the reciprocal computations to get 64 bits of
// precision.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.

	.text
	.align 16
	.global __divdi3
	.proc __divdi3
__divdi3:
	.regstk 2,0,0,0
	// Transfer inputs to FP registers.
	setf.sig f8 = in0
	setf.sig f9 = in1
	;;
	// Convert the inputs to FP, so that they won't be treated as unsigned.
	fcvt.xf f8 = f8
	fcvt.xf f9 = f9
	;;
	// Compute the reciprocal approximation.
	frcpa.s1 f10, p6 = f8, f9
	;;
	// 3 Newton-Raphson iterations.
(p6)	fma.s1 f11 = farg0, f10, f0
(p6)	fnma.s1 f12 = farg1, f10, f1
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f13 = f12, f12, f0
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fma.s1 f11 = f13, f11, f11
(p6)	fma.s1 f12 = f13, f13, f0
(p6)	fma.s1 f10 = f13, f10, f10
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fnma.s1 f8 = f9, f11, f8
	;;
(p6)	fma.s1 f10 = f8, f10, f11
	;;
	// Round quotient to an integer.
	fcvt.fx.trunc.s1 f8 = f10
	;;
	// Transfer result to GP registers.
	getf.sig ret0 = f8
	br.ret.sptk rp
	;;
	.endp __divdi3
#endif

#ifdef L__moddi3
// Compute a 64-bit integer modulus.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 3 iterations
// to get more than the 64 bits of precision that we need for DImode.
//
// Must use max precision for the reciprocal computations to get 64 bits of
// precision.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.

	.text
	.align 16
	.global __moddi3
	.proc __moddi3
__moddi3:
	.regstk 2,0,0,0
	// Transfer inputs to FP registers.
	setf.sig f8 = in0
	setf.sig f9 = in1
	;;
	// Convert the inputs to FP, so that they won't be treated as unsigned.
	fcvt.xf f8 = f8
	fcvt.xf f9 = f9
	;;
	// Compute the reciprocal approximation.
	frcpa.s1 f10, p6 = f8, f9
	;;
	// 3 Newton-Raphson iterations.
(p6)	fma.s1 f11 = farg0, f10, f0
(p6)	fnma.s1 f12 = farg1, f10, f1
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f13 = f12, f12, f0
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fma.s1 f11 = f13, f11, f11
(p6)	fma.s1 f12 = f13, f13, f0
(p6)	fma.s1 f10 = f13, f10, f10
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fnma.s1 f12 = f9, f11, f8
	;;
(p6)	fma.s1 f10 = f12, f10, f11
	;;
	// Round quotient to an integer.
	fcvt.fx.trunc.s1 f10 = f10
	;;
	// Renormalize.
	fcvt.xf f10 = f10
	;;
	// Compute remainder.
	fnma.s1 f8 = f10, f9, f8
	;;
	// Round remainder to an integer.
	fcvt.fx.trunc.s1 f8 = f8
	;;
	// Transfer result to GP registers.
	getf.sig ret0 = f8
	br.ret.sptk rp
	;;
	.endp __moddi3
#endif

#ifdef L__udivdi3
// Compute a 64-bit unsigned integer quotient.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 3 iterations
// to get more than the 64 bits of precision that we need for DImode.
//
// Must use max precision for the reciprocal computations to get 64 bits of
// precision.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.

	.text
	.align 16
	.global __udivdi3
	.proc __udivdi3
__udivdi3:
	.regstk 2,0,0,0
	// Transfer inputs to FP registers.
	setf.sig f8 = in0
	setf.sig f9 = in1
	;;
	// Convert the inputs to FP, to avoid FP software-assist faults.
	fcvt.xuf.s1 f8 = f8
	fcvt.xuf.s1 f9 = f9
	;;
	// Compute the reciprocal approximation.
	frcpa.s1 f10, p6 = f8, f9
	;;
	// 3 Newton-Raphson iterations.
(p6)	fma.s1 f11 = farg0, f10, f0
(p6)	fnma.s1 f12 = farg1, f10, f1
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f13 = f12, f12, f0
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fma.s1 f11 = f13, f11, f11
(p6)	fma.s1 f12 = f13, f13, f0
(p6)	fma.s1 f10 = f13, f10, f10
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fnma.s1 f8 = f9, f11, f8
	;;
(p6)	fma.s1 f10 = f8, f10, f11
	;;
	// Round quotient to an unsigned integer.
	fcvt.fxu.trunc.s1 f8 = f10
	;;
	// Transfer result to GP registers.
	getf.sig ret0 = f8
	br.ret.sptk rp
	;;
	.endp __udivdi3
#endif

#ifdef L__umoddi3
// Compute a 64-bit unsigned integer modulus.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 3 iterations
// to get more than the 64 bits of precision that we need for DImode.
//
// Must use max precision for the reciprocal computations to get 64 bits of
// precision.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.

	.text
	.align 16
	.global __umoddi3
	.proc __umoddi3
__umoddi3:
	.regstk 2,0,0,0
	// Transfer inputs to FP registers.
	setf.sig f8 = in0
	setf.sig f9 = in1
	;;
	// Convert the inputs to FP, to avoid FP software assist faults.
	fcvt.xuf.s1 f8 = f8
	fcvt.xuf.s1 f9 = f9
	;;
	// Compute the reciprocal approximation.
	frcpa.s1 f10, p6 = f8, f9
	;;
	// 3 Newton-Raphson iterations.
(p6)	fma.s1 f11 = farg0, f10, f0
(p6)	fnma.s1 f12 = farg1, f10, f1
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f13 = f12, f12, f0
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fma.s1 f11 = f13, f11, f11
(p6)	fma.s1 f12 = f13, f13, f0
(p6)	fma.s1 f10 = f13, f10, f10
	;;
(p6)	fma.s1 f11 = f12, f11, f11
(p6)	fma.s1 f10 = f12, f10, f10
	;;
(p6)	fnma.s1 f12 = f9, f11, f8
	;;
(p6)	fma.s1 f10 = f12, f10, f11
	;;
	// Round quotient to an unsigned integer.
	fcvt.fxu.trunc.s1 f10 = f10
	;;
	// Renormalize.
	fcvt.xuf.s1 f10 = f10
	;;
	// Compute remainder.
	fnma.s1 f8 = f10, f9, f8
	;;
	// Round remainder to an integer.
	fcvt.fxu.trunc.s1 f8 = f8
	;;
	// Transfer result to GP registers.
	getf.sig ret0 = f8
	br.ret.sptk rp
	;;
	.endp __umoddi3
#endif

#ifdef L__divsi3
// Compute a 32-bit integer quotient.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 2 iterations
// to get more than the 32 bits of precision that we need for SImode.
//
// ??? This is currently not used.  It needs to be fixed to be more like the
// above DImode routines.
//
// ??? Check to see if the error is less than >.5ulp error.  We may need
// some adjustment code to get precise enough results.
//
// ??? Should probably use max precision for the reciprocal computations.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.

	.text
	.align 16
	.global __divsi3
	.proc __divsi3
__divsi3:
	.regstk 2,0,0,0
	setf.sig f8 = in0
	setf.sig f9 = in1
	;;
	fcvt.xf f8 = f8
	fcvt.xf f9 = f9
	;;
	frcpa f11, p6 = f8, f9
	fadd f10 = f1, f1
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fmpy f8 = f8, f11
	;;
	fcvt.fx.trunc f8 = f8
	;;
	getf.sig ret0 = f8
	br.ret.sptk rp
	;;
	.endp __divsi3
#endif

#ifdef L__modsi3
// Compute a 32-bit integer modulus.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 2 iterations
// to get more than the 32 bits of precision that we need for SImode.
//
// ??? This is currently not used.  It needs to be fixed to be more like the
// above DImode routines.
//
// ??? Check to see if the error is less than >.5ulp error.  We may need
// some adjustment code to get precise enough results.
//
// ??? Should probably use max precision for the reciprocal computations.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.

	.text
	.align 16
	.global __modsi3
	.proc __modsi3
__modsi3:
	.regstk 2,0,0,0
	setf.sig f8 = r32
	setf.sig f9 = r33
	;;
	fcvt.xf f8 = f8
	fcvt.xf f9 = f9
	;;
	frcpa f11, p6 = f8, f9
	fadd f10 = f1, f1
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fmpy f10 = f8, f11
	;;
	fcvt.fx.trunc f10 = f10
	;;
	fcvt.xf f10 = f10
	;;
	fnma f8 = f10, f9, f8
	;;
	fcvt.fx f8 = f8
	;;
	getf.sig r32 = f8
	br.ret.sptk rp
	;;
	.endp __modsi3
#endif

#ifdef L__udivsi3
// Compute a 32-bit unsigned integer quotient.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 2 iterations
// to get more than the 32 bits of precision that we need for SImode.
//
// ??? This is currently not used.  It needs to be fixed to be more like the
// above DImode routines.
//
// ??? Check to see if the error is less than >.5ulp error.  We may need
// some adjustment code to get precise enough results.
//
// ??? Should probably use max precision for the reciprocal computations.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.
//
// This is the same as divsi3, except that we don't need fcvt instructions
// before the frcpa.

	.text
	.align 16
	.global __udivsi3
	.proc __udivsi3
__udivsi3:
	.regstk 2,0,0,0
	setf.sig f8 = r32
	setf.sig f9 = r33
	;;
	frcpa f11, p6 = f8, f9
	fadd f10 = f1, f1
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fmpy f8 = f8, f11
	;;
	fcvt.fxu.trunc f8 = f8
	;;
	getf.sig ret0 = f8
	br.ret.sptk rp
	;;
	.endp __udivsi3
#endif

#ifdef L__umodsi3
// Compute a 32-bit unsigned integer modulus.
//
// Use reciprocal approximation and Newton-Raphson iteration to compute the
// quotient.  frcpa gives 8.6 significant bits, so we need 2 iterations
// to get more than the 32 bits of precision that we need for SImode.
//
// ??? This is currently not used.  It needs to be fixed to be more like the
// above DImode routines.
//
// ??? Check to see if the error is less than >.5ulp error.  We may need
// some adjustment code to get precise enough results.
//
// ??? Should probably use max precision for the reciprocal computations.
//
// r32/f8 holds the dividend.  r33/f9 holds the divisor.
// f10 holds the value 2.0.  f11 holds the reciprocal approximation.
// f12 is a temporary.
//
// This is the same as modsi3, except that we don't need fcvt instructions
// before the frcpa.

	.text
	.align 16
	.global __umodsi3
	.proc __umodsi3
__umodsi3:
	.regstk 2,0,0,0
	setf.sig f8 = r32
	setf.sig f9 = r33
	;;
	frcpa f11, p6 = f8, f9
	fadd f10 = f1, f1
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fnma f12 = f9, f11, f10
	;;
	fmpy f11 = f11, f12
	;;
	fmpy f10 = f8, f11
	;;
	fcvt.fxu.trunc f10 = f10
	;;
	fcvt.xuf f10 = f10
	;;
	fnma f8 = f10, f9, f8
	;;
	fcvt.fxu f8 = f8
	;;
	getf.sig r32 = f8
	br.ret.sptk rp
	;;
	.endp __umodsi3
#endif

#ifdef L__save_stack_nonlocal
// Notes on save/restore stack nonlocal: We read ar.bsp but write
// ar.bspstore.  This is because ar.bsp can be read at all times
// (independent of the RSE mode) but since it's read-only we need to
// restore the value via ar.bspstore.  This is OK because
// ar.bsp==ar.bspstore after executing "flushrs".

// void __ia64_save_stack_nonlocal(void *save_area, void *stack_pointer)

	.text
	.align 16
	.global __ia64_save_stack_nonlocal
	.proc __ia64_save_stack_nonlocal
__ia64_save_stack_nonlocal:
	alloc r18=ar.pfs,2,0,0,0
	st8 [in0]=in1,8
	mov r19=ar.rsc
	;;
	flushrs
	and r19=0x1c,r19
	mov ar.pfs=r18
	;;
	mov ar.rsc=r19
	mov r16=ar.bsp
	adds r2=16,in0
	;;
	mov r17=ar.rnat
	st8 [in0]=r16,8
	or r19=0x3,r19
	;;
	st8 [in0]=r17
	mov ar.rsc=r19
	st8 [r2]=r18
	mov ar.pfs=r18
	br.ret.sptk.few rp
	;;
	.endp __ia64_save_stack_nonlocal
#endif

#ifdef L__nonlocal_goto
// void __ia64_nonlocal_goto(void *fp, void *target_label, void *save_area,
//			     void *static_chain);

	.text
	.align 16
	.global __ia64_nonlocal_goto
	.proc __ia64_nonlocal_goto
__ia64_nonlocal_goto:
	alloc r20=ar.pfs,4,0,0,0
	mov r19=ar.rsc
	adds r2=8,in2
	ld8 r12=[in2],16
	mov.ret.sptk rp = r33, .L0
	;;
	flushrs
	ld8 r16=[r2],16
	and r19=0x1c,r19
	ld8 r17=[in2]
	;;
	ld8 r18=[r2]
	mov ar.rsc=r19
	;;
	mov ar.bspstore=r16
	;;
	mov ar.rnat=r17
	mov ar.pfs=r18
	or r19=0x3,r19
	;;
	loadrs
	invala
	mov r7=r32
.L0:	{
	mov ar.rsc=r19
	mov r15=r35
	br.ret.sptk.few rp
	}
	;;
	.endp __ia64_nonlocal_goto
#endif

#ifdef L__restore_stack_nonlocal
// This is mostly the same as nonlocal_goto above.
// ??? This has not been tested yet.

// void __ia64_restore_stack_nonlocal(void *save_area)

	.text
	.align 16
	.global __ia64_restore_stack_nonlocal
	.proc __ia64_restore_stack_nonlocal
__ia64_restore_stack_nonlocal:
	alloc r20=ar.pfs,4,0,0,0
	mov r19=ar.rsc
	adds r2=8,in0
	ld8 r12=[in0],16
	;;
	flushrs
	ld8 r16=[r2],16
	and r19=0x1c,r19
	ld8 r17=[in0]
	;;
	ld8 r18=[r2]
	mov ar.rsc=r19
	;;
	mov ar.bspstore=r16
	;;
	mov ar.rnat=r17
	mov ar.pfs=r18
	or r19=0x3,r19
	;;
	loadrs
	invala
.L0:	{
	mov ar.rsc=r19
	br.ret.sptk.few rp
	}
	;;
	.endp __ia64_restore_stack_nonlocal
#endif
