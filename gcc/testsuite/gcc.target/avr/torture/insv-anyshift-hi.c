/* { dg-do run } */
/* { dg-additional-options { -fno-split-wide-types } } */

typedef __UINT16_TYPE__ uint16_t;

/* Testing inlined and completely folded versions of functions
   against their non-inlined, non-folded counnterparts.  */

#define MK_FUN1(OBIT, LSR)						\
  static __inline__ __attribute__((__always_inline__))			\
  uint16_t fun1_lsr_##OBIT##_##LSR##_ai (int x, uint16_t a)		\
  {									\
    (void) x;								\
    return (a >> LSR) & (1u << OBIT);					\
  }									\
									\
  __attribute__((__noinline__,__noclone__))				\
  uint16_t fun1_lsr_##OBIT##_##LSR##_ni (int x, uint16_t a)		\
  {									\
    return fun1_lsr_##OBIT##_##LSR##_ai (x, a);				\
  }									\
									\
  void test_fun1_lsr_##OBIT##_##LSR (void)				\
  {									\
    if (fun1_lsr_##OBIT##_##LSR##_ni (0, 1u << (OBIT + LSR))		\
	!= fun1_lsr_##OBIT##_##LSR##_ai (0, 1u << (OBIT + LSR)))	\
      __builtin_abort();						\
									\
    if (fun1_lsr_##OBIT##_##LSR##_ni (0, 1u << (OBIT + LSR))		\
	!= fun1_lsr_##OBIT##_##LSR##_ai (0, -1u))			\
      __builtin_abort();						\
  }

#define MK_FUN3(OBIT, LSR)						\
  static __inline__ __attribute__((__always_inline__))			\
  uint16_t fun3_lsr_##OBIT##_##LSR##_ai (uint16_t a)			\
  {									\
    return (a >> LSR) & (1u << OBIT);					\
  }									\
									\
  __attribute__((__noinline__,__noclone__))				\
  uint16_t fun3_lsr_##OBIT##_##LSR##_ni (uint16_t a)			\
  {									\
    return fun3_lsr_##OBIT##_##LSR##_ai (a);				\
  }									\
									\
  void test_fun3_lsr_##OBIT##_##LSR (void)				\
  {									\
    if (fun3_lsr_##OBIT##_##LSR##_ni (1u << (OBIT + LSR))		\
	!= fun3_lsr_##OBIT##_##LSR##_ai (1u << (OBIT + LSR)))		\
      __builtin_abort();						\
									\
    if (fun3_lsr_##OBIT##_##LSR##_ni (1u << (OBIT + LSR))		\
	!= fun3_lsr_##OBIT##_##LSR##_ai (-1u))				\
      __builtin_abort();						\
  }


#define MK_FUN2(OBIT, LSL)						\
  static __inline__ __attribute__((__always_inline__))			\
  uint16_t fun2_lsl_##OBIT##_##LSL##_ai (uint16_t a)			\
  {									\
    return (a << LSL) & (1u << OBIT);					\
  }									\
									\
  __attribute__((__noinline__,__noclone__))				\
  uint16_t fun2_lsl_##OBIT##_##LSL##_ni (uint16_t a)			\
  {									\
    return fun2_lsl_##OBIT##_##LSL##_ai (a);				\
  }									\
									\
  void test_fun2_lsl_##OBIT##_##LSL (void)				\
  {									\
    if (fun2_lsl_##OBIT##_##LSL##_ni (1u << (OBIT - LSL))		\
	!= fun2_lsl_##OBIT##_##LSL##_ai (1u << (OBIT - LSL)))		\
      __builtin_abort();						\
									\
    if (fun2_lsl_##OBIT##_##LSL##_ni (1u << (OBIT - LSL))		\
	!= fun2_lsl_##OBIT##_##LSL##_ai (-1u))				\
      __builtin_abort();						\
  }


MK_FUN1 (10, 4)
MK_FUN1 (6, 1)
MK_FUN1 (1, 5)
MK_FUN1 (0, 8)
MK_FUN1 (0, 4)
MK_FUN1 (0, 1)
MK_FUN1 (0, 0)

MK_FUN3 (10, 4)
MK_FUN3 (6, 1)
MK_FUN3 (1, 5)
MK_FUN3 (0, 8)
MK_FUN3 (0, 4)
MK_FUN3 (0, 1)
MK_FUN3 (0, 0)

MK_FUN2 (12, 8)
MK_FUN2 (15, 15)
MK_FUN2 (14, 12)
MK_FUN2 (8, 8)
MK_FUN2 (7, 4)
MK_FUN2 (5, 4)
MK_FUN2 (5, 1)
MK_FUN2 (4, 0)
MK_FUN2 (1, 0)
MK_FUN2 (0, 0)

int main (void)
{
  test_fun1_lsr_10_4 ();
  test_fun1_lsr_6_1 ();
  test_fun1_lsr_1_5 ();
  test_fun1_lsr_0_8 ();
  test_fun1_lsr_0_4 ();
  test_fun1_lsr_0_1 ();
  test_fun1_lsr_0_0 ();

  test_fun3_lsr_10_4 ();
  test_fun3_lsr_6_1 ();
  test_fun3_lsr_1_5 ();
  test_fun3_lsr_0_8 ();
  test_fun3_lsr_0_4 ();
  test_fun3_lsr_0_1 ();
  test_fun3_lsr_0_0 ();

  test_fun2_lsl_12_8 ();
  test_fun2_lsl_15_15 ();
  test_fun2_lsl_14_12 ();
  test_fun2_lsl_8_8 ();
  test_fun2_lsl_7_4 ();
  test_fun2_lsl_5_4 ();
  test_fun2_lsl_5_1 ();
  test_fun2_lsl_4_0 ();
  test_fun2_lsl_1_0 ();
  test_fun2_lsl_0_0 ();

  return 0;
}
