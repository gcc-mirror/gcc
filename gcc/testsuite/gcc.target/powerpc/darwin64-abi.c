/* Darwin 64-bit ABI testing */
/* { dg-do run { target { powerpc*-*-darwin* && lp64 } } } */
/* { dg-options "-std=c99 -maltivec" } */

/* Set this if 8-byte structs are being passed as integers.  */
/* #define STRUCT8INT */

#include <stdarg.h>
#include <stdio.h>
#include <complex.h>
#include <altivec.h>

extern void abort (void);

struct s3c { char ch[3]; };
struct ssc { short sh; char ch; };
struct sif { int i; float f; };
struct sfi { float f; int i; };
struct sfii { float f; int i; int j; };
struct sfil { float f; int i; long l; };
struct sfif { float f; int i; float g; };
struct sfill { float f; int i; long l, m; };
struct sfl { float f; long l; };
struct sfldl { float f; long l1; double d; long l2; };
struct sfpp { float f; char *p1; char *p2; };


struct sff { float f1, f2; };
struct sfff { float f1, f2, f3; };
struct sffff { float f1, f2, f3, f4; };

struct sfD { float f; long double D; };

struct sidi { int i1; double d; int i2; };

struct sdd { double d1, d2; };
struct sddd { double d1, d2, d3; };
struct sdddd { double d1, d2, d3, d4; };
struct s3d { double d[3]; };

struct vr { union { int ielts[4]; float felts[4]; } elts; };

typedef struct
{
  unsigned long gprs[32];
  double fprs[32];
  struct vr vrs[32];
  unsigned char stack[1000];
} reg_parms_t;

reg_parms_t gparms;

#define TESTFN(RET,NAME,PARAMS) \
RET NAME PARAMS;  \
RET dummy_ ## NAME PARAMS  \
{  \
  __asm__("b end_" #NAME "\n_" # NAME ":\n\t" SAVE_STATE "b _dummy_" # NAME "\n\tend_" #NAME ":\n\n" ); \
}

#define SAVE_STATE \
SAVE_GPR(0)  \
SAVE_GPR(1)  \
SAVE_GPR(3)  \
SAVE_GPR(4)  \
SAVE_GPR(5)  \
SAVE_GPR(6)  \
SAVE_GPR(7)  \
SAVE_GPR(8)  \
SAVE_GPR(9)  \
SAVE_GPR(10)  \
SAVE_FPR(0)  \
SAVE_FPR(1)  \
SAVE_FPR(2)  \
SAVE_FPR(3)  \
SAVE_FPR(4)  \
SAVE_FPR(5)  \
SAVE_FPR(6)  \
SAVE_FPR(7)  \
SAVE_FPR(8)  \
SAVE_FPR(9)  \
SAVE_FPR(10)  \
SAVE_FPR(12)  \
SAVE_FPR(13)  \
SAVE_VR(0)  \
SAVE_VR(1)  \
SAVE_VR(2)  \
SAVE_VR(3)  \
SAVE_VR(4)  \
SAVE_STACK(112)  \
SAVE_STACK(120)  \
SAVE_STACK(128)  \
SAVE_STACK(136)  \
SAVE_STACK(144)  \


#ifdef __LP64__
#define SAVE_GPR(N) "std r" #N "," #N "*8(r25)\n\t"
#define SAVE_FPR(N) "stfd f" #N "," #N "*8+256(r25)\n\t"
#define SAVE_VR(N) "li r26," #N "*16+512\n\tstvx v" #N ",r25,r26\n\t"
#define SAVE_STACK(N) "ld r26," #N "(r1)\n\tstd r26," #N "+1024(r25)\n\t"
#else
#define SAVE_GPR(N) "stw r" #N "," #N "*4(r25)\n\t"
#define SAVE_FPR(N) "stfd f" #N "," #N "*8+128(r25)\n\t"
#define SAVE_VR(N)
#define SAVE_STACK(N)
#endif

TESTFN(void, fffi, (float x, float y, int z))

#define clearall \
__asm__ volatile ( \
"\n\t" \
"li r3,0x333\n\t" \
"li r4,0x444 \n\t" \
"li r5,0x555\n\t" \
"li r6,0x666\n\t" \
"li r7,0x777\n\t" \
"li r8,0x888\n\t" \
"li r9,0x999\n\t" \
"li r10,0xaaa\n\t" \
"fsub f0,f0,f0\n\t" \
"fsub f1,f1,f1\n\t" \
"fsub f2,f2,f2\n\t" \
"fsub f3,f3,f3\n\t" \
"fsub f4,f4,f4\n\t" \
"fsub f5,f5,f5\n\t" \
"fsub f6,f6,f6\n\t" \
"fsub f7,f7,f7\n\t" \
"vsubuwm v0,v0,v0\n\t" \
"vsubuwm v1,v1,v1\n\t" \
"vsubuwm v2,v2,v2\n\t" \
"vsubuwm v3,v3,v3\n\t" \
"vsubuwm v4,v4,v4\n\t" \
: : : "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", \
      "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", \
      "v0", "v1", "v2", "v3", "v4" );

TESTFN(void, fii, (int a, int b))
TESTFN(void, fid, (int i, double d))
TESTFN(void, fc, (complex float z))
TESTFN(void, fffff, (float f1, float f2, float f3, float f4))
TESTFN(void, fdddd, (double d1, double d2, double d3, double d4))
TESTFN(void, f_s3c_ssc, (struct s3c s1, struct ssc s2))
TESTFN(void, f_sff, (struct sff s))
TESTFN(void, f_sfff, (struct sfff s))
TESTFN(void, f_sffff, (struct sffff s))
TESTFN(void, f_sdd, (struct sdd s))
TESTFN(void, f_sddd, (struct sddd s))
TESTFN(void, f_sdddd, (struct sdddd s))
TESTFN(void, f_s3d, (struct s3d s))
TESTFN(void, f_sif, (int i, struct sif s))
TESTFN(void, fi_sif, (int i, struct sif s))
TESTFN(void, fi_sif_i, (int i, struct sif s, int j))
TESTFN(void, f_sfi, (int i, struct sfi s))
TESTFN(void, fi_sfi, (int i, struct sfi s))
TESTFN(void, fi_sfi_if, (int i, struct sfi s, int j, float f))
TESTFN(void, fi_sfill, (int i, struct sfill s))
TESTFN(void, fi_sfill_i, (int i, struct sfill s, int j))
TESTFN(void, f_sfl, (struct sfl s))
TESTFN(void, f_sfl_sfl_sfl_sfl_sfl, (struct sfl s1, struct sfl s2, struct sfl s3, struct sfl s4, struct sfl s5))
TESTFN(void, fi_sff, (int i, struct sff s))
TESTFN(void, f_sfpp_p, (struct sfpp s, char *p))
TESTFN(void, f_sfldl, (struct sfldl s))
TESTFN(void, fi_sff_i, (int i, struct sff s, int j))
TESTFN(void, f_sfD_sfD_sfD_sfD_sfD, (struct sfD s1, struct sfD s2, struct sfD s3, struct sfD s4, struct sfD s5))
TESTFN(void, fi_sidi, (int i, struct sidi s))
TESTFN(void, fifvf_sfi_dots, (int i, float f, vector float vf, struct sfi s, ...))
TESTFN(void, fifvf_sfii_dots, (int i, float f, vector float vf, struct sfii s, ...))

int numerrs;

#ifndef SKIP
static __attribute__ ((noinline)) void
check_gpr (int line, int reg, long expected)
{
  if (gparms.gprs[reg] != expected)
    {
      printf("%d: r%d is 0x%lx, expected 0x%lx\n",
	     line, reg, gparms.gprs[reg], expected);
      ++numerrs;
    }
}

static __attribute__ ((noinline)) void
check_gpr_double (int line, int reg, double expected)
{
  double tmp = *((double *) &(gparms.gprs[reg]));
  if (tmp != expected)
    {
      printf("%d: r%d is %f (0x%llx), expected %f (0x%llx)\n",
	     line, reg,
	     tmp, *((long long *) &tmp),
	     expected, *((long long *) &expected));
      ++numerrs;
    }
}

static __attribute__ ((noinline)) void
check_gpr_float_pair (int line, int reg, float exp1, float exp2)
{
  float tmp1 = *((float *) &(gparms.gprs[reg]));
  float tmp2 = *(((float *) &(gparms.gprs[reg])) + 1);

  if (tmp1 != exp1 || tmp2 != exp2)
    {
      printf("%d: r%d is %f / %f (0x%llx), expected %f (0x%x) / %f (0x%x)\n",
	     line, reg,
	     tmp1, tmp2, *((long long *) &(gparms.gprs[reg])),
	     exp1, *((int *) &exp1),
	     exp2, *((int *) &exp2));
      ++numerrs;
    }
}

static __attribute__ ((noinline)) void
check_fpr (int line, int reg, double expected)
{
  if (gparms.fprs[reg] != expected)
    {
      printf("%d: f%d is %f (0x%llx), expected %f (0x%llx)\n",
	     line, reg,
	     gparms.fprs[reg], *((long long *) &(gparms.fprs[reg])),
	     expected, *((long long *) &expected));
      ++numerrs;
    }
}

static __attribute__ ((noinline)) void
check_vr_int (int reg, int n1, int n2, int n3, int n4)
{
  if (gparms.vrs[reg].elts.ielts[0] != n1
      || gparms.vrs[reg].elts.ielts[1] != n2
      || gparms.vrs[reg].elts.ielts[2] != n3
      || gparms.vrs[reg].elts.ielts[3] != n4)
    {
      printf("v%d is   (%d,%d,%d,%d) (0x%x,0x%x,0x%x,0x%x),\n"
	     " expected (%d,%d,%d,%d) (0x%x,0x%x,0x%x,0x%x)\n",
	     reg,
	     gparms.vrs[reg].elts.ielts[0],
	     gparms.vrs[reg].elts.ielts[1],
	     gparms.vrs[reg].elts.ielts[2],
	     gparms.vrs[reg].elts.ielts[3],
	     gparms.vrs[reg].elts.ielts[0],
	     gparms.vrs[reg].elts.ielts[1],
	     gparms.vrs[reg].elts.ielts[2],
	     gparms.vrs[reg].elts.ielts[3],
	     n1, n2, n3, n4,
	     n1, n2, n3, n4
	     );
      ++numerrs;
    }
}

static __attribute__ ((noinline)) void
check_vr_float (int reg, float f1, float f2, float f3, float f4)
{
  if (gparms.vrs[reg].elts.felts[0] != f1
      || gparms.vrs[reg].elts.felts[1] != f2
      || gparms.vrs[reg].elts.felts[2] != f3
      || gparms.vrs[reg].elts.felts[3] != f4)
    {
      printf("v%d is    (%f,%f,%f,%f) (0x%x,0x%x,0x%x,0x%x),\n"
	     " expected (%f,%f,%f,%f) (0x%x,0x%x,0x%x,0x%x)\n",
	     reg,
	     gparms.vrs[reg].elts.felts[0],
	     gparms.vrs[reg].elts.felts[1],
	     gparms.vrs[reg].elts.felts[2],
	     gparms.vrs[reg].elts.felts[3],
	     gparms.vrs[reg].elts.ielts[0],
	     gparms.vrs[reg].elts.ielts[1],
	     gparms.vrs[reg].elts.ielts[2],
	     gparms.vrs[reg].elts.ielts[3],
	     f1, f2, f3, f4,
	     *((int *) &f1), *((int *) &f2), *((int *) &f3), *((int *) &f4)
	     );
      ++numerrs;
    }
}
#endif

int main (void)
{
  complex float cpx = 4.45f + I * 4.92f;
  struct s3c s3c_loc;
  struct ssc ssc_loc;
  struct sfi sfi_loc;
  struct sfi sfi_loc2 = { 6.3f, 0x1108 };
  struct sfii sfii_loc;
  struct sfii sfii_loc2 = { 6.9f, 0x1110, 0x6372 };
  vector float vf_loc = (vector float) { 7.1f, 7.2f, 7.3f, 7.4f };
  vector int vi_loc = (vector int) { 0xabc, 0xdef, 0xfed, 0xcba };

  __asm__ ("mr r25,%0" : : "b" (&gparms) );

  clearall;
  fii(1, 2);
  check_gpr (__LINE__, 3, 1);
  check_gpr (__LINE__, 4, 2);

  clearall;
  fid(45, 4.5);
  check_gpr (__LINE__, 3, 45);
  check_fpr (__LINE__, 1, 4.5);

  clearall;
  fffi(1.2f, 3.4f, 456);
  check_fpr(__LINE__, 1, 1.2f);

  clearall;
  fc(cpx);
  /* Two floats are packed into r3 */
  check_gpr_float_pair (__LINE__, 3, 4.45f, 4.92f);

  clearall;
  fffff (4.1f, 4.2f, 4.3f, 4.4f);
  check_fpr (__LINE__, 1, 4.1f);
  check_fpr (__LINE__, 4, 4.4f);

  clearall;
  fdddd (4.1, 4.2, 4.3, 4.4);
  check_fpr (__LINE__, 1, 4.1);
  check_fpr (__LINE__, 4, 4.4);

  {
    struct sff sff_loc = { 2.1f, 2.2f };
    clearall;
    f_sff(sff_loc);
#ifdef STRUCT8INT
    check_gpr_float_pair (__LINE__, 3, 2.1f, 2.2f);
#else
    check_fpr(__LINE__, 1, 2.1f);
    check_fpr(__LINE__, 2, 2.2f);
#endif
    clearall;
    fi_sff_i(65, sff_loc, 66);
    check_gpr(__LINE__, 3, 65);
#ifdef STRUCT8INT
    check_gpr_float_pair (__LINE__, 4, 2.1f, 2.2f);
#else
    check_fpr(__LINE__, 1, 2.1f);
    check_fpr(__LINE__, 2, 2.2f);
#endif
    check_gpr(__LINE__, 5, 66);
  }

  {
    struct sfff sfff_loc = { 3.1f, 3.2f, 3.3f };
    clearall;
    f_sfff(sfff_loc);
    check_fpr(__LINE__, 1, 3.1f);
    check_fpr(__LINE__, 2, 3.2f);
    check_fpr(__LINE__, 3, 3.3f);
    clearall;
    f_sfff(sfff_loc);
    check_fpr(__LINE__, 1, 3.1f);
    check_fpr(__LINE__, 2, 3.2f);
    check_fpr(__LINE__, 3, 3.3f);
  }

  {
    struct sffff sffff_loc = { 4.1f, 4.2f, 4.3f, 4.4f };
    clearall;
    f_sffff(sffff_loc);
    check_gpr_float_pair(__LINE__, 3, 4.1f, 4.2f);
    check_gpr_float_pair(__LINE__, 4, 4.3f, 4.4f);
  }

  {
    struct sdd sdd_loc = { 2.1, 2.2 };
    clearall;
    f_sdd(sdd_loc);
    /* 16-byte struct is passed in two GPRs.  */
    check_gpr_double(__LINE__, 3, 2.1);
    check_gpr_double(__LINE__, 4, 2.2);
  }

  {
    struct sddd sddd_loc = { 3.1, 3.2, 3.3 };
    clearall;
    f_sddd(sddd_loc);
    check_fpr(__LINE__, 1, 3.1);
    check_fpr(__LINE__, 2, 3.2);
    check_fpr(__LINE__, 3, 3.3);
  }

  {
    struct sdddd sdddd_loc = { 4.1, 4.2, 4.3, 4.4 };
    clearall;
    f_sdddd(sdddd_loc);
    check_fpr(__LINE__, 1, 4.1);
    check_fpr(__LINE__, 2, 4.2);
    check_fpr(__LINE__, 3, 4.3);
    check_fpr(__LINE__, 4, 4.4);
  }

  {
    struct s3d s3d_loc = { 89.92, 4.89, 90.9 };
    clearall;
    f_s3d(s3d_loc);
    check_gpr_double (__LINE__, 3, 89.92);
    check_gpr_double (__LINE__, 4, 4.89);
    check_gpr_double (__LINE__, 5, 90.9);
  }

  {
    s3c_loc.ch[0] = 'A';
    s3c_loc.ch[1] = 'B';
    s3c_loc.ch[2] = 'C';
    ssc_loc.sh = 0x1234;
    ssc_loc.ch = 'D';
    clearall;
    f_s3c_ssc(s3c_loc, ssc_loc);
  }

  {
    struct sif sif_loc_n = { 334, 4.3f };
    long floatcast;
    floatcast = *((int *) &(sif_loc_n.f));
    clearall;
    fi_sif(29, sif_loc_n);
    check_gpr (__LINE__, 3, 29);
    check_gpr (__LINE__, 4, 334LL << 32 | floatcast);
#ifdef STRUCT8INT
#else
    check_fpr (__LINE__, 1, 4.3f);
#endif
    clearall;
    fi_sif_i(31, sif_loc_n, 33);
    check_gpr (__LINE__, 3, 31);
    check_gpr (__LINE__, 4, 334LL << 32 | floatcast);
#ifdef STRUCT8INT
#else
    check_fpr (__LINE__, 1, 4.3f);
#endif
    check_gpr (__LINE__, 5, 33);
  }

  {
    struct sfi sfi_loc_n = { 4.145f, 335 };
    clearall;
    fi_sfi(29, sfi_loc_n);
    check_gpr (__LINE__, 3, 29);
#ifdef STRUCT8INT
    check_gpr (__LINE__, 4, 0x4084a3d70000014fLL);
#else
    check_fpr (__LINE__, 1, 4.145f);
    check_gpr (__LINE__, 4, 335);
#endif
  }

  {
    struct sfi sfi_loc_n = { 4.145f, 335 };
    clearall;
    fi_sfi_if (29, sfi_loc_n, 65, 9.8f);
    check_gpr (__LINE__, 3, 29);
#ifdef STRUCT8INT
    check_gpr (__LINE__, 4, 0x4084a3d70000014fLL);
#else
    check_fpr (__LINE__, 1, 4.145f);
    check_gpr (__LINE__, 4, 335);
#endif
    check_gpr (__LINE__, 5, 65);
    check_gpr (__LINE__, 6, 0x666);
#ifdef STRUCT8INT
    check_fpr (__LINE__, 1, 9.8f);
#else
    check_fpr (__LINE__, 2, 9.8f);
#endif
    check_gpr (__LINE__, 7, 0x777);
  }

  {
    struct sfill sfill_loc_n = { 4.145f, 335, 10000000000LL, 20000000000LL };
    clearall;
    fi_sfill(29, sfill_loc_n);
    check_gpr (__LINE__, 3, 29);
    check_fpr (__LINE__, 1, 4.145f);
    check_gpr (__LINE__, 4, 335);
    check_gpr (__LINE__, 5, 10000000000LL);
    check_gpr (__LINE__, 6, 20000000000LL);
  }

  {
    struct sfl sfl_loc_n = { 4.145f, 335 };
    clearall;
    f_sfl (sfl_loc_n);
    check_gpr_float_pair (__LINE__, 3, 4.145f, 0.0f);
    check_gpr (__LINE__, 4, 335);
    check_gpr (__LINE__, 5, 0x555);
    clearall;
    f_sfl_sfl_sfl_sfl_sfl (sfl_loc_n, sfl_loc_n, sfl_loc_n, sfl_loc_n, sfl_loc_n);
    check_gpr_float_pair (__LINE__, 3, 4.145f, 0.0f);
    check_gpr (__LINE__, 4, 335);
    check_gpr (__LINE__, 6, 335);
    check_gpr (__LINE__, 8, 335);
    check_gpr (__LINE__, 10, 335);
  }

  {
    struct sfldl sfldl_loc_n = { 4.145f, 335, 3.3, 336 };
    clearall;
    f_sfldl (sfldl_loc_n);
    check_fpr (__LINE__, 1, 4.145f);
    check_gpr (__LINE__, 4, 335);
    check_fpr (__LINE__, 2, 3.3);
    check_gpr (__LINE__, 6, 336);
  }

  {
    char *p1 = "abc";
    char *p2 = "def";
    char *p3 = "ghi";
    struct sfpp sfpp_loc_n = { 4.145f, p1, p2 };
    clearall;
    f_sfpp_p(sfpp_loc_n, p3);
    check_fpr (__LINE__, 1, 4.145f);
    check_gpr (__LINE__, 4, (long) p1);
    check_gpr (__LINE__, 5, (long) p2);
    check_gpr (__LINE__, 6, (long) p3);
  }

  {
    struct sff sff_loc_n = { 4.145f, 335.3f };
    clearall;
    fi_sff(29, sff_loc_n);
    check_gpr (__LINE__, 3, 29);
#ifdef STRUCT8INT
    check_gpr_float_pair (__LINE__, 4, 4.145f, 335.3f);
#else
    check_fpr (__LINE__, 1, 4.145f);
    check_fpr (__LINE__, 2, 335.3f);
#endif
  }

  {
    struct sfD sfD_loc_n = { 4.145f, 335.335 };
    clearall;
    f_sfD_sfD_sfD_sfD_sfD (sfD_loc_n, sfD_loc_n, sfD_loc_n, sfD_loc_n, sfD_loc_n);
    check_fpr (__LINE__, 1, 4.145f);
    check_fpr (__LINE__, 2, 335.335);
    check_fpr (__LINE__, 4, 4.145f);
    check_fpr (__LINE__, 5, 335.335);
    check_fpr (__LINE__, 7, 4.145f);
    check_fpr (__LINE__, 10, 4.145f);
    check_fpr (__LINE__, 13, 4.145f);
  }

  {
    struct sidi sidi_loc_n = { 257, 4.14515, 258 };
    clearall;
    fi_sidi(16, sidi_loc_n);
    check_gpr (__LINE__, 3, 16);
    check_fpr (__LINE__, 1, 4.14515);
    check_gpr (__LINE__, 4, 257LL << 32);
    check_gpr (__LINE__, 5, 0x555);
    check_gpr (__LINE__, 6, 258LL << 32);
  }

  sfi_loc.f = 5.2f;
  sfi_loc.i = 98;
  clearall;
  fifvf_sfi_dots(41, 4.3f, vf_loc, sfi_loc, 4.63f, vi_loc, sfi_loc2);
  __asm__ ("\n");
  check_gpr (__LINE__, 3, 41);
  check_fpr (__LINE__, 1, 4.3f); /* float skips r4 */
  check_vr_float(2, 7.1f, 7.2f, 7.3f, 7.4f); /* vector skips r5/r6 */
#ifdef STRUCT8INT
  check_gpr (__LINE__, 7, 0x40a6666600000062);
#else
  check_fpr (__LINE__, 2, sfi_loc.f);
  check_gpr (__LINE__, 7, sfi_loc.i);
#endif
  /* start of varying parameters */
#ifdef STRUCT8INT
  check_fpr (__LINE__, 2, 4.63f);
#else
  check_fpr (__LINE__, 3, 4.63f);
#endif
  check_gpr_double (__LINE__, 8, 4.63f);
  /* vector takes up r9/r10 */
  /* sfi_loc2 on stack */

  clearall;
  sfii_loc.f = 5.2f;
  sfii_loc.i = 98;
  sfii_loc.j = 777;
  clearall;
  fifvf_sfii_dots(41, 4.3f, vf_loc, sfii_loc, 4.63f, vi_loc, sfii_loc2);
  __asm__ ("\n");
  check_gpr (__LINE__, 3, 41);
  check_fpr (__LINE__, 1, 4.3f); /* float skips r4 */
  check_vr_float(2, 7.1f, 7.2f, 7.3f, 7.4f); /* vector skips r5/r6 */
  check_fpr (__LINE__, 2, sfii_loc.f);
  check_gpr (__LINE__, 7, sfii_loc.i);
  check_gpr (__LINE__, 8, ((long)sfii_loc.j) << 32);
  /* start of varying parameters */
  check_fpr (__LINE__, 3, 4.63f);
  check_gpr_double (__LINE__, 9, 4.63f);
  /* vector takes up r10/stack (?) */
  /* sfii_loc2 on stack */

  if (numerrs > 0)
    abort ();
  return 0;
}

int dumpall()
{
  int i;

  printf("\n");
  for (i = 3; i <= 10; ++i)
#ifdef __LP64__
    printf("r%d=0x%16.16lx ", i, gparms.gprs[i]);
#else
    printf("r%d=0x%8.8x ", i, gparms.gprs[i]);
#endif
  printf("\n");
  for (i = 1; i <= 13; ++i)
    printf("f%d=%8.8f ", i, gparms.fprs[i]);
  printf("\n");
  for (i = 0; i <= 4; ++i)
    printf("v%d=(%x,%x,%x,%x)  ", i,
	   gparms.vrs[i].elts.ielts[0], gparms.vrs[i].elts.ielts[1],
	   gparms.vrs[i].elts.ielts[2], gparms.vrs[i].elts.ielts[3]);
  printf("\n");
  for (i = 112; i < 152; ++i)
    {
      if (i > 112 && i % 8 == 0)
	printf(" | ");
      printf("%02x", gparms.stack[i]);
    }
  printf("\n");
}
