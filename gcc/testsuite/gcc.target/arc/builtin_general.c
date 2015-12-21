/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration" } */

#define NORET1OP(name, op1type)			\
  void test_ ## name ## _0 (op1type a)		\
  {						\
    __builtin_arc_ ## name (a);			\
  }						\
  void test_ ## name ## _1 (void)		\
  {						\
    __builtin_arc_ ## name (0x10);		\
  }

#define RET1OP(name, rettype, op1type)		\
  rettype test_ ## name ## _0 (op1type a)	\
  {						\
    return __builtin_arc_ ## name (a);		\
  }						\
  rettype test_ ## name ## _1 (void)		\
  {						\
    return __builtin_arc_ ## name (0x10);	\
  }

NORET1OP (flag, unsigned int)

#if defined (__EM__) || defined (__HS__)
NORET1OP (kflag, unsigned int)
NORET1OP (seti, int)
#endif


#ifdef __ARC_NORM__
RET1OP (norm, int, int)
RET1OP (normw, int, short)
#endif

