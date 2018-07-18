/* { dg-do run { target { powerpc64-*-* && { lp64 && dfprt } } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-std=gnu99 -O2 -fno-strict-aliasing" } */

/* Testcase to check for ABI compliance of parameter passing
   for the PowerPC64 ELF ABI for decimal float values.  */

extern void abort (void);
int failcnt = 0;

/* Support compiling the test to report individual failures; default is
   to abort as soon as a check fails.  */
#ifdef DBG
#include <stdio.h>
#define FAILURE { printf ("failed at line %d\n", __LINE__); failcnt++; }
#else
#define FAILURE abort ();
#endif

typedef struct
{
  int pad;
  _Decimal32 d;
} d32parm_t;

typedef struct
{
  unsigned long gprs[8];
  double fprs[13];
} reg_parms_t;

reg_parms_t gparms;


/* Wrapper to save the GPRs and FPRs and then jump to the real function.  */
#if _CALL_ELF != 2
#define FUNC_START(NAME)						\
	"\t.globl\t" NAME "\n\t"					\
         ".section \".opd\",\"aw\"\n\t"					\
         ".align 3\n"							\
         NAME ":\n\t"							\
         ".quad .L." NAME ",.TOC.@tocbase,0\n\t"			\
         ".text\n\t"							\
         ".type " NAME ", @function\n"					\
         ".L." NAME ":\n\t"
#else
#define FUNC_START(NAME)						\
	"\t.globl\t" NAME "\n\t"					\
         ".text\n\t"							\
         NAME ":\n"							\
	"0:\taddis 2,12,(.TOC.-0b)@ha\n\t"				\
	"addi 2,2,(.TOC.-0b)@l\n\t"					\
	".localentry " NAME ",.-" NAME "\n\t"
#endif
#define WRAPPER(NAME)							\
__asm__ (FUNC_START (#NAME "_asm")					\
	 "ld 11,gparms@got(2)\n\t"					\
	 "std 3,0(11)\n\t"						\
	 "std 4,8(11)\n\t"						\
	 "std 5,16(11)\n\t"						\
	 "std 6,24(11)\n\t"						\
	 "std 7,32(11)\n\t"						\
	 "std 8,40(11)\n\t"						\
	 "std 9,48(11)\n\t"						\
	 "std 10,56(11)\n\t"						\
	 "stfd 1,64(11)\n\t"						\
	 "stfd 2,72(11)\n\t"						\
	 "stfd 3,80(11)\n\t"						\
	 "stfd 4,88(11)\n\t"						\
	 "stfd 5,96(11)\n\t"						\
	 "stfd 6,104(11)\n\t"						\
	 "stfd 7,112(11)\n\t"						\
	 "stfd 8,120(11)\n\t"						\
	 "stfd 9,128(11)\n\t"						\
	 "stfd 10,136(11)\n\t"						\
	 "stfd 11,144(11)\n\t"						\
	 "stfd 12,152(11)\n\t"						\
	 "stfd 13,160(11)\n\t"						\
	 "b " #NAME "\n\t"						\
	 ".long 0\n\t"							\
	 ".byte 0,0,0,0,0,0,0,0\n\t"					\
	 ".size " #NAME ",.-" #NAME "\n")

typedef struct sf
{
  struct sf *backchain;
  long a1;
  long a2;
  long a3;
#if _CALL_ELF != 2
  long a4;
  long a5;
#endif
  unsigned long slot[100];
} stack_frame_t;

extern void func0_asm (double, double, double, double, double, double,
		       double, double, double, double, double, double,
		       double, double, 
		       _Decimal64, _Decimal128, _Decimal64);

WRAPPER(func0);

/* Fill up floating point registers with double arguments, forcing
   decimal float arguments into the parameter save area.  */
void __attribute__ ((noinline))
func0 (double a1, double a2, double a3, double a4, double a5, double a6,
       double a7, double a8, double a9, double a10, double a11, double a12,
       double a13, double a14, 
       _Decimal64 a15, _Decimal128 a16, _Decimal64 a17)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != gparms.fprs[0]) FAILURE
  if (a2 != gparms.fprs[1]) FAILURE
  if (a3 != gparms.fprs[2]) FAILURE
  if (a4 != gparms.fprs[3]) FAILURE
  if (a5 != gparms.fprs[4]) FAILURE
  if (a6 != gparms.fprs[5]) FAILURE
  if (a7 != gparms.fprs[6]) FAILURE
  if (a8 != gparms.fprs[7]) FAILURE
  if (a9 != gparms.fprs[8]) FAILURE
  if (a10 != gparms.fprs[9]) FAILURE
  if (a11 != gparms.fprs[10]) FAILURE
  if (a12 != gparms.fprs[11]) FAILURE
  if (a13 != gparms.fprs[12]) FAILURE
  if (a14 != *(double *)&sp->slot[13]) FAILURE
  if (a15 != *(_Decimal64 *)&sp->slot[14]) FAILURE
  if (a16 != *(_Decimal128 *)&sp->slot[15]) FAILURE
  if (a17 != *(_Decimal64 *)&sp->slot[17]) FAILURE
}

extern void func1_asm (double, double, double, double, double, double,
		       double, double, double, double, double, double,
		       double, _Decimal128 );

WRAPPER(func1);

void __attribute__ ((noinline))
func1 (double a1, double a2, double a3, double a4, double a5, double a6,
       double a7, double a8, double a9, double a10, double a11, double a12,
       double a13, _Decimal128 a14)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != gparms.fprs[0]) FAILURE
  if (a2 != gparms.fprs[1]) FAILURE
  if (a3 != gparms.fprs[2]) FAILURE
  if (a4 != gparms.fprs[3]) FAILURE
  if (a5 != gparms.fprs[4]) FAILURE
  if (a6 != gparms.fprs[5]) FAILURE
  if (a7 != gparms.fprs[6]) FAILURE
  if (a8 != gparms.fprs[7]) FAILURE
  if (a9 != gparms.fprs[8]) FAILURE
  if (a10 != gparms.fprs[9]) FAILURE
  if (a11 != gparms.fprs[10]) FAILURE
  if (a12 != gparms.fprs[11]) FAILURE
  if (a13 != gparms.fprs[12]) FAILURE
  if (a14 != *(_Decimal128 *)&sp->slot[13]) FAILURE
}

extern void func2_asm (double, double, double, double, double, double,
		       double, double, double, double, double, double,
		       _Decimal128);

WRAPPER(func2);

void __attribute__ ((noinline))
func2 (double a1, double a2, double a3, double a4, double a5, double a6,
       double a7, double a8, double a9, double a10, double a11, double a12,
       _Decimal128 a13)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != gparms.fprs[0]) FAILURE
  if (a2 != gparms.fprs[1]) FAILURE
  if (a3 != gparms.fprs[2]) FAILURE
  if (a4 != gparms.fprs[3]) FAILURE
  if (a5 != gparms.fprs[4]) FAILURE
  if (a6 != gparms.fprs[5]) FAILURE
  if (a7 != gparms.fprs[6]) FAILURE
  if (a8 != gparms.fprs[7]) FAILURE
  if (a9 != gparms.fprs[8]) FAILURE
  if (a10 != gparms.fprs[9]) FAILURE
  if (a11 != gparms.fprs[10]) FAILURE
  if (a12 != gparms.fprs[11]) FAILURE
  if (a13 != *(_Decimal128 *)&sp->slot[12]) FAILURE
}

extern void func3_asm (_Decimal64, _Decimal128, _Decimal64, _Decimal128,
		       _Decimal64, _Decimal128, _Decimal64, _Decimal128,
		       _Decimal64, _Decimal128);

WRAPPER(func3);

void __attribute__ ((noinline))
func3 (_Decimal64 a1, _Decimal128 a2, _Decimal64 a3, _Decimal128 a4,
       _Decimal64 a5, _Decimal128 a6, _Decimal64 a7, _Decimal128 a8,
       _Decimal64 a9, _Decimal128 a10)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != *(_Decimal64 *)&gparms.fprs[0]) FAILURE	/* f1        */
  if (a2 != *(_Decimal128 *)&gparms.fprs[1]) FAILURE	/* f2 & f3   */
  if (a3 != *(_Decimal64 *)&gparms.fprs[3]) FAILURE	/* f4        */
  if (a4 != *(_Decimal128 *)&gparms.fprs[5]) FAILURE	/* f6 & f7   */
  if (a5 != *(_Decimal64 *)&gparms.fprs[7]) FAILURE	/* f8        */
  if (a6 != *(_Decimal128 *)&gparms.fprs[9]) FAILURE	/* f10 & f11 */
  if (a7 != *(_Decimal64 *)&gparms.fprs[11]) FAILURE	/* f12       */
  if (a8 != *(_Decimal128 *)&sp->slot[10]) FAILURE
  if (a9 != *(_Decimal64 *)&sp->slot[12]) FAILURE
  if (a10 != *(_Decimal128 *)&sp->slot[13]) FAILURE
}

extern void func4_asm (_Decimal128, _Decimal64, _Decimal128, _Decimal64,
		       _Decimal128, _Decimal64, _Decimal128, _Decimal64);

WRAPPER(func4);

void __attribute__ ((noinline))
func4 (_Decimal128 a1, _Decimal64 a2, _Decimal128 a3, _Decimal64 a4,
       _Decimal128 a5, _Decimal64 a6, _Decimal128 a7, _Decimal64 a8)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != *(_Decimal128 *)&gparms.fprs[1]) FAILURE	/* f2 & f3   */
  if (a2 != *(_Decimal64 *)&gparms.fprs[3]) FAILURE	/* f4        */
  if (a3 != *(_Decimal128 *)&gparms.fprs[5]) FAILURE	/* f6 & f7   */
  if (a4 != *(_Decimal64 *)&gparms.fprs[7]) FAILURE	/* f8        */
  if (a5 != *(_Decimal128 *)&gparms.fprs[9]) FAILURE	/* f10 & f11 */
  if (a6 != *(_Decimal64 *)&gparms.fprs[11]) FAILURE	/* f12       */
  if (a7 != *(_Decimal128 *)&sp->slot[9]) FAILURE
  if (a8 != *(_Decimal64 *)&sp->slot[11]) FAILURE
}

extern void func5_asm (_Decimal32, _Decimal32, _Decimal32, _Decimal32,
		       _Decimal32, _Decimal32, _Decimal32, _Decimal32,
		       _Decimal32, _Decimal32, _Decimal32, _Decimal32,
		       _Decimal32, _Decimal32, _Decimal32, _Decimal32);

WRAPPER(func5);

void __attribute__ ((noinline))
func5 (_Decimal32 a1, _Decimal32 a2, _Decimal32 a3, _Decimal32 a4,
       _Decimal32 a5, _Decimal32 a6, _Decimal32 a7, _Decimal32 a8,
       _Decimal32 a9, _Decimal32 a10, _Decimal32 a11, _Decimal32 a12,
       _Decimal32 a13, _Decimal32 a14, _Decimal32 a15, _Decimal32 a16)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  /* _Decimal32 is passed in the lower half of an FPR or parameter slot.  */
  if (a1 != ((d32parm_t *)&gparms.fprs[0])->d) FAILURE		/* f1  */
  if (a2 != ((d32parm_t *)&gparms.fprs[1])->d) FAILURE		/* f2  */
  if (a3 != ((d32parm_t *)&gparms.fprs[2])->d) FAILURE		/* f3  */
  if (a4 != ((d32parm_t *)&gparms.fprs[3])->d) FAILURE		/* f4  */
  if (a5 != ((d32parm_t *)&gparms.fprs[4])->d) FAILURE		/* f5  */
  if (a6 != ((d32parm_t *)&gparms.fprs[5])->d) FAILURE		/* f6  */
  if (a7 != ((d32parm_t *)&gparms.fprs[6])->d) FAILURE		/* f7  */
  if (a8 != ((d32parm_t *)&gparms.fprs[7])->d) FAILURE		/* f8  */
  if (a9 != ((d32parm_t *)&gparms.fprs[8])->d) FAILURE		/* f9  */
  if (a10 != ((d32parm_t *)&gparms.fprs[9])->d) FAILURE		/* f10 */
  if (a11 != ((d32parm_t *)&gparms.fprs[10])->d) FAILURE	/* f11 */
  if (a12 != ((d32parm_t *)&gparms.fprs[11])->d) FAILURE	/* f12 */
  if (a13 != ((d32parm_t *)&gparms.fprs[12])->d) FAILURE	/* f13 */
  if (a14 != ((d32parm_t *)&sp->slot[13])->d) FAILURE
  if (a15 != ((d32parm_t *)&sp->slot[14])->d) FAILURE
  if (a16 != ((d32parm_t *)&sp->slot[15])->d) FAILURE
}

extern void func6_asm (_Decimal32, _Decimal64, _Decimal128,
		       _Decimal32, _Decimal64, _Decimal128,
		       _Decimal32, _Decimal64, _Decimal128,
		       _Decimal32, _Decimal64, _Decimal128);

WRAPPER(func6);

void __attribute__ ((noinline))
func6 (_Decimal32 a1, _Decimal64 a2, _Decimal128 a3,
       _Decimal32 a4, _Decimal64 a5, _Decimal128 a6,
       _Decimal32 a7, _Decimal64 a8, _Decimal128 a9,
       _Decimal32 a10, _Decimal64 a11, _Decimal128 a12)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != ((d32parm_t *)&gparms.fprs[0])->d) FAILURE		/* f1        */
  if (a2 != *(_Decimal64 *)&gparms.fprs[1]) FAILURE		/* f2        */
  if (a3 != *(_Decimal128 *)&gparms.fprs[3]) FAILURE		/* f4 & f5   */
  if (a4 != ((d32parm_t *)&gparms.fprs[5])->d) FAILURE		/* f6        */
  if (a5 != *(_Decimal64 *)&gparms.fprs[6]) FAILURE		/* f7        */
  if (a6 != *(_Decimal128 *)&gparms.fprs[7]) FAILURE		/* f8 & f9   */
  if (a7 != ((d32parm_t *)&gparms.fprs[9])->d) FAILURE		/* f10       */
  if (a8 != *(_Decimal64 *)&gparms.fprs[10]) FAILURE		/* f11       */
  if (a9 != *(_Decimal128 *)&gparms.fprs[11]) FAILURE		/* f12 & f13 */
  if (a10 != ((d32parm_t *)&sp->slot[12])->d) FAILURE
  if (a11 != *(_Decimal64 *)&sp->slot[13]) FAILURE
}

int
main (void)
{
  func0_asm (1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 12.5, 13.5,
	     14.5, 15.2dd, 16.2dl, 17.2dd);
  func1_asm (101.5, 102.5, 103.5, 104.5, 105.5, 106.5, 107.5, 108.5, 109.5,
	     110.5, 111.5, 112.5, 113.5, 114.2dd);
  func2_asm (201.5, 202.5, 203.5, 204.5, 205.5, 206.5, 207.5, 208.5, 209.5,
	     210.5, 211.5, 212.5, 213.2dd);
  func3_asm (301.2dd, 302.2dl, 303.2dd, 304.2dl, 305.2dd, 306.2dl, 307.2dd,
	     308.2dl, 309.2dd, 310.2dl);
  func4_asm (401.2dl, 402.2dd, 403.2dl, 404.2dd, 405.2dl, 406.2dd, 407.2dl,
	     408.2dd);
#if 0
  /* _Decimal32 doesn't yet follow the ABI; enable this when it does.  */
  func5_asm (501.2df, 502.2df, 503.2df, 504.2df, 505.2df, 506.2df, 507.2df,
	     508.2df, 509.2df, 510.2df, 511.2df, 512.2df, 513.2df, 514.2df,
	     515.2df, 516.2df);
  func6_asm (601.2df, 602.2dd, 603.2dl, 604.2df, 605.2dd, 606.2dl,
	     607.2df, 608.2dd, 609.2dl, 610.2df, 611.2dd, 612.2dl);
#endif

  if (failcnt != 0)
    abort ();

  return 0;
}
