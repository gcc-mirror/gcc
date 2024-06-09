/* { dg-do run } */

typedef __UINT32_TYPE__ uint32_t;

/* Testing inlined and completely folded versions of functions
   against their non-inlined, non-folded counnterparts.  */

#define MK_FUN1(OBIT, LSR)						\
  static __inline__ __attribute__((__always_inline__))			\
  uint32_t fun1_lsr_##OBIT##_##LSR##_ai (int x, uint32_t a)		\
  {									\
    (void) x;								\
    return (a >> LSR) & (1ul << OBIT);					\
  }									\
									\
  __attribute__((__noinline__,__noclone__))				\
  uint32_t fun1_lsr_##OBIT##_##LSR##_ni (int x, uint32_t a)		\
  {									\
    return fun1_lsr_##OBIT##_##LSR##_ai (x, a);				\
  }									\
									\
  void test_fun1_lsr_##OBIT##_##LSR (void)				\
  {									\
    if (fun1_lsr_##OBIT##_##LSR##_ni (0, 1ul << (OBIT + LSR))		\
	!= fun1_lsr_##OBIT##_##LSR##_ai (0, 1ul << (OBIT + LSR)))	\
      __builtin_abort();						\
									\
    if (fun1_lsr_##OBIT##_##LSR##_ni (0, 1ul << (OBIT + LSR))		\
	!= fun1_lsr_##OBIT##_##LSR##_ai (0, -1ul))			\
      __builtin_abort();						\
  }
  

#define MK_FUN2(OBIT, LSL)						\
  static __inline__ __attribute__((__always_inline__))			\
  uint32_t fun2_lsl_##OBIT##_##LSL##_ai (int x, uint32_t a)		\
  {									\
    (void) x;								\
    return (a << LSL) & (1ul << OBIT);					\
  }									\
									\
  __attribute__((__noinline__,__noclone__))				\
  uint32_t fun2_lsl_##OBIT##_##LSL##_ni (int x, uint32_t a)		\
  {									\
    return fun2_lsl_##OBIT##_##LSL##_ai (x, a);				\
  }									\
									\
  void test_fun2_lsl_##OBIT##_##LSL (void)				\
  {									\
    if (fun2_lsl_##OBIT##_##LSL##_ni (0, 1ul << (OBIT - LSL))		\
	!= fun2_lsl_##OBIT##_##LSL##_ai (0, 1ul << (OBIT - LSL)))	\
      __builtin_abort();						\
									\
    if (fun2_lsl_##OBIT##_##LSL##_ni (0, 1ul << (OBIT - LSL))		\
	!= fun2_lsl_##OBIT##_##LSL##_ai (0, -1ul))			\
      __builtin_abort();						\
  }


MK_FUN1 (13, 15)
MK_FUN1 (13, 16)
MK_FUN1 (13, 17)
MK_FUN1 (13, 12)
MK_FUN1 (0, 31)
MK_FUN1 (0, 8)
MK_FUN1 (0, 0)

MK_FUN2 (12, 8)
MK_FUN2 (13, 8)
MK_FUN2 (16, 8)
MK_FUN2 (16, 0)

int main (void)
{
  test_fun1_lsr_13_15 ();
  test_fun1_lsr_13_16 ();
  test_fun1_lsr_13_17 ();
  test_fun1_lsr_13_12 ();
  test_fun1_lsr_0_31 ();
  test_fun1_lsr_0_8 ();
  test_fun1_lsr_0_0 ();

  test_fun2_lsl_12_8 ();
  test_fun2_lsl_13_8 ();
  test_fun2_lsl_16_8 ();
  test_fun2_lsl_16_0 ();

  return 0;
}
