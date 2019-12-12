/* { dg-do compile } */
/* { dg-options "-O2 -Werror-implicit-function-declaration" } */

#define NORET(name)				\
  void test_ ## name (void)			\
  {						\
    __builtin_arc_ ## name ();			\
  }

#define RET(name, rettype)			\
  rettype test_ ## name (void)			\
  {						\
    return __builtin_arc_ ## name ();		\
  }

#define NORET1OP(name, op1type)			\
  void test_ ## name ## _1 (void)		\
  {						\
    __builtin_arc_ ## name (0x10);		\
  }


NORET (nop)
#if !defined (__ARC600__) && !defined (__ARC601__)
NORET (rtie)
#endif

#ifdef __A7__
 NORET (sync)
#endif

NORET (brk)
NORET (swi)

NORET1OP (sleep, unsigned int)

#if defined (__A7__) || defined (__EM__) || defined (__HS__)
NORET1OP (trap_s, unsigned int)
NORET (unimp_s)
#endif

#if defined (__EM__) || defined (__HS__)
RET (clri, int)
#endif
