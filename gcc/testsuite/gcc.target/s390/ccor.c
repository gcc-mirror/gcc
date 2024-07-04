/* { dg-do compile } */
/* { dg-options "-O3 -march=zEC12 -mzarch" } */

#define GENFUN1(C)                              \
  int foo_ ##C(int x)                           \
  {                                             \
    int cc;					\
    asm volatile ("ahi %[x],42\n"		\
		  : [x] "+d"(x), "=@cc" (cc));	\
    return cc == C ? 42 : 0;			\
  }

#define GENFUN2(C1,C2)                          \
  int foo_ ##C1##C2(int x)                      \
  {                                             \
    int cc;					\
    asm volatile ("ahi %[x],42\n"		\
		  : [x] "+d"(x), "=@cc" (cc));	\
    return cc == C1 || cc == C2 ? 42 : 0;	\
  }

#define GENFUN3(C1,C2,C3)                                               \
  int foo_ ##C1##C2##C3(int x)                                          \
  {                                                                     \
    int cc;                                                             \
    asm volatile ("ahi %[x],42\n"					\
		  : [x] "+d"(x), "=@cc" (cc));				\
    return cc == C1 || cc == C2 || cc == C3 ? 42 : 0;			\
  }

GENFUN1(0)

/* { dg-final { scan-assembler {locrne} } } */

GENFUN1(1)

/* { dg-final { scan-assembler {locrnl} } } */

GENFUN1(2)

/* { dg-final { scan-assembler {locrnh} } } */

GENFUN1(3)

/* { dg-final { scan-assembler {locro} } } */

GENFUN2(0,1)

/* { dg-final { scan-assembler {locrnle} } } */

GENFUN2(0,2)

/* { dg-final { scan-assembler {locrhe} } } */

GENFUN2(0,3)

/* currently unoptimized */

GENFUN2(1,2)

/* { dg-final { scan-assembler {locrlh} } } */

GENFUN2(1,3)

/* { dg-final { scan-assembler {locrnhe} } } */

GENFUN2(2,3)

/* { dg-final { scan-assembler {locrle} } } */

GENFUN3(0,1,2)

/* { dg-final { scan-assembler {locrh} } } */

GENFUN3(0,1,3)

/* currently unoptimized */

GENFUN3(0,2,3)

/* currently unoptimized */

GENFUN3(1,2,3)

/* { dg-final { scan-assembler {locre} } } */

/* for the unoptimized cases, we get an ipm */
/* { dg-final { scan-assembler-times {ipm} 3 } } */
