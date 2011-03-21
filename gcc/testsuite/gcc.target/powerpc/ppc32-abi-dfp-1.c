/* { dg-do run { target { powerpc_fprs && { ilp32 && dfprt } } } } */
/* { dg-options "-std=gnu99 -O2 -fno-strict-aliasing" } */

/* Testcase to check for ABI compliance of parameter passing
   for the PowerPC ELF ABI for decimal float values.  */

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
  unsigned int gprs[8];
  double fprs[8];
} reg_parms_t;

reg_parms_t gparms;

typedef struct sf
{
  struct sf *backchain;
  int a1;
  unsigned int slot[200];
} stack_frame_t;

/* Wrapper to save the GPRs and FPRs and then jump to the real function.  */
#define WRAPPER(NAME)							\
__asm__ ("\t.globl\t" #NAME "_asm\n\t"					\
	 ".text\n\t"							\
	 ".type " #NAME "_asm, @function\n"				\
	 #NAME "_asm:\n\t"						\
	 "lis 11,gparms@ha\n\t"						\
	 "la 11,gparms@l(11)\n\t"					\
	 "st 3,0(11)\n\t"						\
	 "st 4,4(11)\n\t"						\
	 "st 5,8(11)\n\t"						\
	 "st 6,12(11)\n\t"						\
	 "st 7,16(11)\n\t"						\
	 "st 8,20(11)\n\t"						\
	 "st 9,24(11)\n\t"						\
	 "st 10,28(11)\n\t"						\
	 "stfd 1,32(11)\n\t"						\
	 "stfd 2,40(11)\n\t"						\
	 "stfd 3,48(11)\n\t"						\
	 "stfd 4,56(11)\n\t"						\
	 "stfd 5,64(11)\n\t"						\
	 "stfd 6,72(11)\n\t"						\
	 "stfd 7,80(11)\n\t"						\
	 "stfd 8,88(11)\n\t"						\
	 "b " #NAME "\n\t"						\
	 ".size " #NAME ",.-" #NAME "\n")

/* Fill up floating point registers with double arguments, forcing
   decimal float arguments into the parameter save area.  */
extern void func0_asm (double, double, double, double, double,
		       double, double, double, _Decimal64, _Decimal128);

WRAPPER(func0);

void __attribute__ ((noinline))
func0 (double a1, double a2, double a3, double a4, double a5,
       double a6, double a7, double a8, _Decimal64 a9, _Decimal128 a10)
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
  if (a9 != *(_Decimal64 *)&sp->slot[0]) FAILURE
  if (a10 != *(_Decimal128 *)&sp->slot[2]) FAILURE
}

/* Alternate 64-bit and 128-bit decimal float arguments, checking that
   _Decimal128 is always passed in even/odd register pairs.  */
extern void func1_asm (_Decimal64, _Decimal128, _Decimal64, _Decimal128,
		       _Decimal64, _Decimal128, _Decimal64, _Decimal128);

WRAPPER(func1);

void __attribute__ ((noinline))
func1 (_Decimal64 a1, _Decimal128 a2, _Decimal64 a3, _Decimal128 a4,
       _Decimal64 a5, _Decimal128 a6, _Decimal64 a7, _Decimal128 a8)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != *(_Decimal64 *)&gparms.fprs[0]) FAILURE	/* f1 */
  if (a2 != *(_Decimal128 *)&gparms.fprs[1]) FAILURE	/* f2 & f3 */
  if (a3 != *(_Decimal64 *)&gparms.fprs[3]) FAILURE	/* f4 */
  if (a4 != *(_Decimal128 *)&gparms.fprs[5]) FAILURE	/* f6 & f7 */
  if (a5 != *(_Decimal64 *)&gparms.fprs[7]) FAILURE	/* f8 */
  if (a6 != *(_Decimal128 *)&sp->slot[0]) FAILURE
  if (a7 != *(_Decimal64 *)&sp->slot[4]) FAILURE
  if (a8 != *(_Decimal128 *)&sp->slot[6]) FAILURE
}

extern void func2_asm (_Decimal128, _Decimal64, _Decimal128, _Decimal64,
		       _Decimal128, _Decimal64, _Decimal128, _Decimal64);

WRAPPER(func2);

void __attribute__ ((noinline))
func2 (_Decimal128 a1, _Decimal64 a2, _Decimal128 a3, _Decimal64 a4,
       _Decimal128 a5, _Decimal64 a6, _Decimal128 a7, _Decimal64 a8)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != *(_Decimal128 *)&gparms.fprs[1]) FAILURE	/* f2 & f3 */
  if (a2 != *(_Decimal64 *)&gparms.fprs[3]) FAILURE	/* f4 */
  if (a3 != *(_Decimal128 *)&gparms.fprs[5]) FAILURE	/* f6 & f7 */
  if (a4 != *(_Decimal64 *)&gparms.fprs[7]) FAILURE	/* f8 */
  if (a5 != *(_Decimal128 *)&sp->slot[0]) FAILURE
  if (a6 != *(_Decimal64 *)&sp->slot[4]) FAILURE
  if (a7 != *(_Decimal128 *)&sp->slot[6]) FAILURE
  if (a8 != *(_Decimal64 *)&sp->slot[10]) FAILURE
}

extern void func3_asm (_Decimal64, _Decimal128, _Decimal64, _Decimal128,
		       _Decimal64);

WRAPPER(func3);

void __attribute__ ((noinline))
func3 (_Decimal64 a1, _Decimal128 a2, _Decimal64 a3, _Decimal128 a4,
       _Decimal64 a5)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != *(_Decimal64 *)&gparms.fprs[0]) FAILURE	/* f1 */
  if (a2 != *(_Decimal128 *)&gparms.fprs[1]) FAILURE	/* f2 & f3 */
  if (a3 != *(_Decimal64 *)&gparms.fprs[3]) FAILURE	/* f4 */
  if (a4 != *(_Decimal128 *)&gparms.fprs[5]) FAILURE	/* f6 & f7 */
  if (a5 != *(_Decimal128 *)&sp->slot[0]) FAILURE
}

extern void func4_asm (_Decimal32, _Decimal32, _Decimal32, _Decimal32,
		       _Decimal32, _Decimal32, _Decimal32, _Decimal32,
		       _Decimal32, _Decimal32, _Decimal32, _Decimal32,
		       _Decimal32, _Decimal32, _Decimal32, _Decimal32);

WRAPPER(func4);

void __attribute__ ((noinline))
func4 (_Decimal32 a1, _Decimal32 a2, _Decimal32 a3, _Decimal32 a4,
       _Decimal32 a5, _Decimal32 a6, _Decimal32 a7, _Decimal32 a8,
       _Decimal32 a9, _Decimal32 a10, _Decimal32 a11, _Decimal32 a12,
       _Decimal32 a13, _Decimal32 a14, _Decimal32 a15, _Decimal32 a16)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  /* _Decimal32 is passed in the lower half of an FPR, or in parameter slot.  */
  if (a1 != ((d32parm_t *)&gparms.fprs[0])->d) FAILURE		/* f1  */
  if (a2 != ((d32parm_t *)&gparms.fprs[1])->d) FAILURE		/* f2  */
  if (a3 != ((d32parm_t *)&gparms.fprs[2])->d) FAILURE		/* f3  */
  if (a4 != ((d32parm_t *)&gparms.fprs[3])->d) FAILURE		/* f4  */
  if (a5 != ((d32parm_t *)&gparms.fprs[4])->d) FAILURE		/* f5  */
  if (a6 != ((d32parm_t *)&gparms.fprs[5])->d) FAILURE		/* f6  */
  if (a7 != ((d32parm_t *)&gparms.fprs[6])->d) FAILURE		/* f7  */
  if (a8 != ((d32parm_t *)&gparms.fprs[7])->d) FAILURE		/* f8  */
  if (a9 != *(_Decimal32 *)&sp->slot[0]) FAILURE
  if (a10 != *(_Decimal32 *)&sp->slot[1]) FAILURE
  if (a11 != *(_Decimal32 *)&sp->slot[2]) FAILURE
  if (a12 != *(_Decimal32 *)&sp->slot[3]) FAILURE
  if (a13 != *(_Decimal32 *)&sp->slot[4]) FAILURE
  if (a14 != *(_Decimal32 *)&sp->slot[5]) FAILURE
  if (a15 != *(_Decimal32 *)&sp->slot[6]) FAILURE
  if (a16 != *(_Decimal32 *)&sp->slot[7]) FAILURE
}

extern void func5_asm (_Decimal32, _Decimal64, _Decimal128,
		       _Decimal32, _Decimal64, _Decimal128,
		       _Decimal32, _Decimal64, _Decimal128,
		       _Decimal32, _Decimal64, _Decimal128);

WRAPPER(func5);

void __attribute__ ((noinline))
func5 (_Decimal32 a1, _Decimal64 a2, _Decimal128 a3,
       _Decimal32 a4, _Decimal64 a5, _Decimal128 a6,
       _Decimal32 a7, _Decimal64 a8, _Decimal128 a9,
       _Decimal32 a10, _Decimal64 a11, _Decimal128 a12)
{
  stack_frame_t *sp;

  sp = __builtin_frame_address (0);
  sp = sp->backchain;

  if (a1 != ((d32parm_t *)&gparms.fprs[0])->d) FAILURE		/* f1      */
  if (a2 != *(_Decimal64 *)&gparms.fprs[1]) FAILURE		/* f2      */
  if (a3 != *(_Decimal128 *)&gparms.fprs[3]) FAILURE		/* f4 & f5 */
  if (a4 != ((d32parm_t *)&gparms.fprs[5])->d) FAILURE		/* f6      */
  if (a5 != *(_Decimal64 *)&gparms.fprs[6]) FAILURE		/* f7      */

  if (a6 != *(_Decimal128 *)&sp->slot[0]) FAILURE
  if (a7 != *(_Decimal32 *)&sp->slot[4]) FAILURE
  if (a8 != *(_Decimal64 *)&sp->slot[6]) FAILURE
  if (a9 != *(_Decimal128 *)&sp->slot[8]) FAILURE
  if (a10 != *(_Decimal32 *)&sp->slot[12]) FAILURE
  if (a11 != *(_Decimal64 *)&sp->slot[14]) FAILURE
  if (a12 != *(_Decimal128 *)&sp->slot[16]) FAILURE
}

int
main ()
{
  func0_asm (1., 2., 3., 4., 5., 6., 7., 8., 9.dd, 10.dl);
  func1_asm (1.dd, 2.dl, 3.dd, 4.dl, 5.dd, 6.dl, 7.dd, 8.dl);
  func2_asm (1.dl, 2.dd, 3.dl, 4.dd, 5.dl, 6.dd, 7.dl, 8.dd);
  func3_asm (1.dd, 2.dl, 3.dd, 4.dl, 5.dl);
  func4_asm (501.2df, 502.2df, 503.2df, 504.2df, 505.2df, 506.2df, 507.2df,
	     508.2df, 509.2df, 510.2df, 511.2df, 512.2df, 513.2df, 514.2df,
	     515.2df, 516.2df);
  func5_asm (601.2df, 602.2dd, 603.2dl, 604.2df, 605.2dd, 606.2dl,
	     607.2df, 608.2dd, 609.2dl, 610.2df, 611.2dd, 612.2dl);

  if (failcnt != 0)
    abort ();

  return 0;
}
