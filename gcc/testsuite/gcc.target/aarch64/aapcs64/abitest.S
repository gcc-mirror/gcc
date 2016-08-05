	.global dumpregs
	.global myfunc
	.type dumpregs,%function
	.type myfunc,%function

#ifdef __LP64__
#define PTR_REG(n)      x##n
#else
#define PTR_REG(n)      w##n
#endif

dumpregs:
myfunc:
      mov	x16, sp
      mov	x17, sp
      sub	sp,  sp, 368 // 352 for registers and 16 for old sp and lr

      sub	x17, x17, 8
      st4	{ v4.h, v5.h, v6.h, v7.h }[0], [x17] //344
      sub	x17, x17, 8
      st4	{ v0.h, v1.h, v2.h, v3.h }[0], [x17] //336

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
      ldr      PTR_REG(9), [x9, #0]
      blr	x9				// function return value test
      adrp	x9, saved_return_address
      add	x9, x9, :lo12:saved_return_address
      ldr	x9, [x9, #0]
      str	x9, [sp, #8]			// Update the copy of LR reg saved on stack
LABEL_RET:
      ldp	x0, x30, [sp]
      mov	sp, x0
      ret

.weak	testfunc
.weak	testfunc_ptr
.weak	saved_return_address
