	.global dumpregs
	.global myfunc
	.type dumpregs,%function
	.type myfunc,%function
dumpregs:
myfunc:
      mov	x16, sp
      mov	x17, sp
      sub	sp,  sp, 352 // 336 for registers and 16 for old sp and lr

      stp	x8, x9, [x17, #-16]! //320

      stp	q6, q7, [x17, #-32]! //288
      stp	q4, q5, [x17, #-32]! //256
      stp	q2, q3, [x17, #-32]! //224
      stp	q0, q1, [x17, #-32]! //192

      stp	x6, x7, [x17, #-16]! //176
      stp	x4, x5, [x17, #-16]! //160
      stp	x2, x3, [x17, #-16]! //144
      stp	x0, x1, [x17, #-16]! //128

      stp	w6, w7, [x17, #-8]!  //120
      stp	w4, w5, [x17, #-8]!  //112
      stp	w2, w3, [x17, #-8]!  //104
      stp	w0, w1, [x17, #-8]!  // 96

      stp	s6, s7, [x17, #-8]!  // 88
      stp	s4, s5, [x17, #-8]!  // 80
      stp	s2, s3, [x17, #-8]!  // 72
      stp	s0, s1, [x17, #-8]!  // 64

      stp	d6, d7, [x17, #-16]! // 48
      stp	d4, d5, [x17, #-16]! // 32
      stp	d2, d3, [x17, #-16]! // 16
      stp	d0, d1, [x17, #-16]! //  0

      add	x0, sp,  #16
      stp	x16, x30, [x17, #-16]!

      adrp	x9, which_kind_of_test		// determine the type of test
      add	x9, x9, :lo12:which_kind_of_test
      ldr	w9, [x9, #0]
      cmp	w9, #1
      bgt	LABEL_TEST_FUNC_RETURN
      bl	testfunc			// parameter passing test or va_arg code gen test
      b		LABEL_RET
LABEL_TEST_FUNC_RETURN:
      adrp	x9, testfunc_ptr
      add	x9, x9, :lo12:testfunc_ptr
      ldr	x9, [x9, #0]
      blr	x9				// function return value test
LABEL_RET:
      ldp	x0, x30, [sp]
      mov	sp, x0
      ret

.weak	testfunc
.weak	testfunc_ptr
