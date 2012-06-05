/* { dg-do run } */
/* { dg-options "-fno-inline -fomit-frame-pointer" } */
/* { dg-additional-options "-mdynamic-no-pic" { target *-*-darwin* } } */

/* -fno-inline -maltivec -m32/-m64 -mmultiple/no-multiple -Os/-O2.  */
#ifndef NO_BODY
#define abort() __builtin_abort ()
#define vec_all_eq(v1,v2) __builtin_vec_vcmpeq_p (2, v1, v2)
#define SET(T,R,V) register T R __asm__ (#R) = V
#define SET_GPR(R,V) SET (long, R, V)
#define SET_FPR(R,V) SET (double, R, V)
#define SET_VR(R,V) SET (__attribute__ ((vector_size (16))) int, R, V)
#define SET_CR(R,V) __asm__ __volatile__ ("mtcrf %0,%1" : : "n" (1<<(7-R)), "r" (V<<(4*(7-R))) : "cr" #R)
#define TRASH_GPR(R) SET_GPR (R, 0)
#define TRASH_FPR(R) SET_FPR (R, 0)
#define TRASH_VR(R) SET_VR (R, val0)
#define TRASH_CR(R) SET_CR (R, 0)
#define TRASH_SOME_GPR TRASH_GPR (r30); TRASH_GPR (r31)
#define TRASH_SOME_FPR TRASH_FPR (fr28); TRASH_FPR (fr31)
#define TRASH_SOME_VR TRASH_VR (v26); TRASH_VR (v27); TRASH_VR (v31)
#define TRASH_SOME_CR TRASH_CR (2)
#define TRASH_ALL_GPR TRASH_GPR (r14); TRASH_GPR (r15); TRASH_GPR (r16); TRASH_GPR (r17); TRASH_GPR (r18); TRASH_GPR (r19); TRASH_GPR (r20); TRASH_GPR (r21); TRASH_GPR (r22); TRASH_GPR (r23); TRASH_GPR (r24); TRASH_GPR (r25); TRASH_GPR (r26); TRASH_GPR (r27); TRASH_GPR (r28); TRASH_GPR (r29); TRASH_GPR (r30); TRASH_GPR (r31)
#define TRASH_ALL_FPR TRASH_FPR (fr14); TRASH_FPR (fr15); TRASH_FPR (fr16); TRASH_FPR (fr17); TRASH_FPR (fr18); TRASH_FPR (fr19); TRASH_FPR (fr20); TRASH_FPR (fr21); TRASH_FPR (fr22); TRASH_FPR (fr23); TRASH_FPR (fr24); TRASH_FPR (fr25); TRASH_FPR (fr26); TRASH_FPR (fr27); TRASH_FPR (fr28); TRASH_FPR (fr29); TRASH_FPR (fr30); TRASH_FPR (fr31)
#define TRASH_ALL_VR TRASH_VR (v20); TRASH_VR (v21); TRASH_VR (v22); TRASH_VR (v23); TRASH_VR (v24); TRASH_VR (v25); TRASH_VR (v26); TRASH_VR (v27); TRASH_VR (v28); TRASH_VR (v29); TRASH_VR (v30); TRASH_VR (v31)
#define TRASH_ALL_CR TRASH_CR (2); TRASH_CR (3); TRASH_CR (4)
#define USE_SOME_GPR __asm__ __volatile__ ("#%0 %1" : : "r" (r30), "r" (r31))
#define USE_SOME_FPR __asm__ __volatile__ ("#%0 %1" : : "f" (fr28), "f" (fr31))
#define USE_SOME_VR __asm__ __volatile__ ("#%0 %1 %2" : : "v" (v26), "v" (v27), "v" (v31))
#define USE_SOME_CR
#define USE_ALL_GPR __asm__ __volatile__ ("#%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15 %16 %17" : : "r" (r14), "r" (r15), "r" (r16), "r" (r17), "r" (r18), "r" (r19), "r" (r20), "r" (r21), "r" (r22), "r" (r23), "r" (r24), "r" (r25), "r" (r26), "r" (r27), "r" (r28), "r" (r29), "r" (r30), "r" (r31))
#define USE_ALL_FPR __asm__ __volatile__ ("#%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11 %12 %13 %14 %15 %16 %17" : : "f" (fr14), "f" (fr15), "f" (fr16), "f" (fr17), "f" (fr18), "f" (fr19), "f" (fr20), "f" (fr21), "f" (fr22), "f" (fr23), "f" (fr24), "f" (fr25), "f" (fr26), "f" (fr27), "f" (fr28), "f" (fr29), "f" (fr30), "f" (fr31))
#define USE_ALL_VR __asm__ __volatile__ ("#%0 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10 %11" : : "v" (v20), "v" (v21), "v" (v22), "v" (v23), "v" (v24), "v" (v25), "v" (v26), "v" (v27), "v" (v28), "v" (v29), "v" (v30), "v" (v31))
#define USE_ALL_CR

#define INIT_GPR SET_GPR (r14, 14); SET_GPR (r15, 15); SET_GPR (r16, 16); SET_GPR (r17, 17); SET_GPR (r18, 18); SET_GPR (r19, 19); SET_GPR (r20, 20); SET_GPR (r21, 21); SET_GPR (r22, 22); SET_GPR (r23, 23); SET_GPR (r24, 24); SET_GPR (r25, 25); SET_GPR (r26, 26); SET_GPR (r27, 27); SET_GPR (r28, 28); SET_GPR (r29, 29); SET_GPR (r30, 30); SET_GPR (r31, 31)
#define INIT_FPR SET_FPR (fr14, 140.0); SET_FPR (fr15, 150.0); SET_FPR (fr16, 160.0); SET_FPR (fr17, 170.0); SET_FPR (fr18, 180.0); SET_FPR (fr19, 190.0); SET_FPR (fr20, 200.0); SET_FPR (fr21, 210.0); SET_FPR (fr22, 220.0); SET_FPR (fr23, 230.0); SET_FPR (fr24, 240.0); SET_FPR (fr25, 250.0); SET_FPR (fr26, 260.0); SET_FPR (fr27, 270.0); SET_FPR (fr28, 280.0); SET_FPR (fr29, 290.0); SET_FPR (fr30, 300.0); SET_FPR (fr31, 310.0)
#define INIT_VR SET_VR (v20, val20); SET_VR (v21, val21); SET_VR (v22, val22); SET_VR (v23, val23); SET_VR (v24, val24); SET_VR (v25, val25); SET_VR (v26, val26); SET_VR (v27, val27); SET_VR (v28, val28); SET_VR (v29, val29); SET_VR (v30, val30); SET_VR (v31, val31)
#define INIT_CR SET_CR (2, 6); SET_CR (3, 7); SET_CR (4, 8)
#ifdef __ALTIVEC__
__attribute__ ((vector_size (16))) int val0 = {0,0,0,0};
__attribute__ ((vector_size (16))) int val20 = {-201,-202,-203,-204};
__attribute__ ((vector_size (16))) int val21 = {-211,-212,-213,-214};
__attribute__ ((vector_size (16))) int val22 = {-221,-222,-223,-224};
__attribute__ ((vector_size (16))) int val23 = {-231,-232,-233,-234};
__attribute__ ((vector_size (16))) int val24 = {-241,-242,-243,-244};
__attribute__ ((vector_size (16))) int val25 = {-251,-252,-253,-254};
__attribute__ ((vector_size (16))) int val26 = {-261,-262,-263,-264};
__attribute__ ((vector_size (16))) int val27 = {-271,-272,-273,-274};
__attribute__ ((vector_size (16))) int val28 = {-281,-282,-283,-284};
__attribute__ ((vector_size (16))) int val29 = {-291,-292,-293,-294};
__attribute__ ((vector_size (16))) int val30 = {-301,-302,-303,-304};
__attribute__ ((vector_size (16))) int val31 = {-311,-312,-313,-314};
#define INIT_REGS INIT_VR; INIT_FPR; INIT_GPR; INIT_CR
#else
#ifndef __NO_FPRS__
#define INIT_REGS INIT_FPR; INIT_GPR; INIT_CR
#else
#define INIT_REGS INIT_GPR; INIT_CR
#endif
#endif
#define VERIFY_GPR if (r14 != 14 || r15 != 15 || r16 != 16 || r17 != 17 || r18 != 18 || r19 != 19 || r20 != 20 || r21 != 21 || r22 != 22 || r23 != 23 || r24 != 24 || r25 != 25 || r26 != 26 || r27 != 27 || r28 != 28 || r29 != 29 || r30 != 30 || r31 != 31) abort ()
#define VERIFY_FPR if (fr14 != 140.0 || fr15 != 150.0 || fr16 != 160.0 || fr17 != 170.0 || fr18 != 180.0 || fr19 != 190.0 || fr20 != 200.0 || fr21 != 210.0 || fr22 != 220.0 || fr23 != 230.0 || fr24 != 240.0 || fr25 != 250.0 || fr26 != 260.0 || fr27 != 270.0 || fr28 != 280.0 || fr29 != 290.0 || fr30 != 300.0 || fr31 != 310.0) abort ()
#define VERIFY_VR if (!vec_all_eq (v20, val20) || !vec_all_eq (v21, val21) || !vec_all_eq (v22, val22) || !vec_all_eq (v23, val23) || !vec_all_eq (v24, val24) || !vec_all_eq (v25, val25) || !vec_all_eq (v26, val26) || !vec_all_eq (v27, val27) || !vec_all_eq (v28, val28) || !vec_all_eq (v29, val29) || !vec_all_eq (v30, val30) || !vec_all_eq (v31, val31)) abort ()
#define VERIFY_CR ({ int tmp; __asm__ __volatile__ ("mfcr %0" : "=r" (tmp)); if ((tmp & ((15 << 20) | (15 << 16) | (15 << 12))) != ((6 << 20) | (7 << 16) | (8 << 12))) abort (); })
#ifdef __ALTIVEC__
#define VERIFY_REGS VERIFY_VR; VERIFY_FPR; VERIFY_GPR; VERIFY_CR
#else
#ifndef __NO_FPRS__
#define VERIFY_REGS VERIFY_FPR; VERIFY_GPR; VERIFY_CR
#else
#define VERIFY_REGS VERIFY_GPR; VERIFY_CR
#endif
#endif

#else /* NO_BODY */
/* For looking at prologue and epilogue code without distractions.  */
#define abort()
#define TRASH_ALL_CR
#define TRASH_ALL_VR
#define TRASH_ALL_FPR
#define TRASH_ALL_GPR
#define USE_ALL_CR
#define USE_ALL_VR
#define USE_ALL_FPR
#define USE_ALL_GPR
#define TRASH_SOME_CR
#define TRASH_SOME_VR
#define TRASH_SOME_FPR
#define TRASH_SOME_GPR
#define USE_SOME_CR
#define USE_SOME_VR
#define USE_SOME_FPR
#define USE_SOME_GPR
#define INIT_REGS
#define VERIFY_REGS
#endif

#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
void b_all (void)
{
  char a[33000];
  TRASH_ALL_CR;
  TRASH_ALL_VR;
  TRASH_ALL_FPR;
  TRASH_ALL_GPR;
  USE_ALL_CR;
  USE_ALL_VR;
  USE_ALL_FPR;
  USE_ALL_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "cr3", "cr4", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "fr14", "fr15", "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23", "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31", "r14", "r15", "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31");
}

void b_cvfr (void)
{
  char a[33000];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_VR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
}

void b_vfr (void)
{
  char a[33000];
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_VR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
}

void b_cvf (void)
{
  char a[33000];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  USE_SOME_CR;
  USE_SOME_VR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31", "fr28", "fr31");
}

void b_vf (void)
{
  char a[33000];
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  USE_SOME_VR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31", "fr28", "fr31");
}
#endif

void b_cvr (void)
{
  char a[33000];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_VR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31", "r30", "r31");
}

void b_vr (void)
{
  char a[33000];
  TRASH_SOME_VR;
  TRASH_SOME_GPR;
  USE_SOME_VR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31", "r30", "r31");
}

void b_cv (void)
{
  char a[33000];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  USE_SOME_CR;
  USE_SOME_VR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31");
}

void b_v (void)
{
  char a[33000];
  TRASH_SOME_VR;
  USE_SOME_VR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31");
}
#endif

#ifndef __NO_FPRS__
void b_cfr (void)
{
  char a[33000];
  TRASH_SOME_CR;
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "fr28", "fr31", "r30", "r31");
}

void b_fr (void)
{
  char a[33000];
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "fr28", "fr31", "r30", "r31");
}

void b_cf (void)
{
  char a[33000];
  TRASH_SOME_CR;
  TRASH_SOME_FPR;
  USE_SOME_CR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "fr28", "fr31");
}

void b_f (void)
{
  char a[33000];
  TRASH_SOME_FPR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "fr28", "fr31");
}
#endif

void b_cr (void)
{
  char a[33000];
  TRASH_SOME_CR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "r30", "r31");
}

void b_r (void)
{
  char a[33000];
  TRASH_SOME_GPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "r30", "r31");
}

void b_c (void)
{
  char a[33000];
  TRASH_SOME_CR;
  USE_SOME_CR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2");
}

void b_0 (void)
{
  char a[33000];
  __asm __volatile ("#%0" : "=m" (a) );
}

#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
void s_all (void)
{
  char a[33];
  TRASH_ALL_CR;
  TRASH_ALL_VR;
  TRASH_ALL_FPR;
  TRASH_ALL_GPR;
  USE_ALL_CR;
  USE_ALL_VR;
  USE_ALL_FPR;
  USE_ALL_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "cr3", "cr4", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "fr14", "fr15", "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23", "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31", "r14", "r15", "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31");
}

void s_cvfr (void)
{
  char a[33];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_VR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
}

void s_vfr (void)
{
  char a[33];
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_VR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
}

void s_cvf (void)
{
  char a[33];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  USE_SOME_CR;
  USE_SOME_VR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31", "fr28", "fr31");
}

void s_vf (void)
{
  char a[33];
  TRASH_SOME_VR;
  TRASH_SOME_FPR;
  USE_SOME_VR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31", "fr28", "fr31");
}
#endif

void s_cvr (void)
{
  char a[33];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_VR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31", "r30", "r31");
}

void s_vr (void)
{
  char a[33];
  TRASH_SOME_VR;
  TRASH_SOME_GPR;
  USE_SOME_VR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31", "r30", "r31");
}

void s_cv (void)
{
  char a[33];
  TRASH_SOME_CR;
  TRASH_SOME_VR;
  USE_SOME_CR;
  USE_SOME_VR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "v26", "v27", "v31");
}

void s_v (void)
{
  char a[33];
  TRASH_SOME_VR;
  USE_SOME_VR;
  __asm __volatile ("#%0" : "=m" (a) : : "v26", "v27", "v31");
}
#endif

#ifndef __NO_FPRS__
void s_cfr (void)
{
  char a[33];
  TRASH_SOME_CR;
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "fr28", "fr31", "r30", "r31");
}

void s_fr (void)
{
  char a[33];
  TRASH_SOME_FPR;
  TRASH_SOME_GPR;
  USE_SOME_FPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "fr28", "fr31", "r30", "r31");
}

void s_cf (void)
{
  char a[33];
  TRASH_SOME_CR;
  TRASH_SOME_FPR;
  USE_SOME_CR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "fr28", "fr31");
}

void s_f (void)
{
  char a[33];
  TRASH_SOME_FPR;
  USE_SOME_FPR;
  __asm __volatile ("#%0" : "=m" (a) : : "fr28", "fr31");
}
#endif

void s_cr (void)
{
  char a[33];
  TRASH_SOME_CR;
  TRASH_SOME_GPR;
  USE_SOME_CR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2", "r30", "r31");
}

void s_r (void)
{
  char a[33];
  TRASH_SOME_GPR;
  USE_SOME_GPR;
  __asm __volatile ("#%0" : "=m" (a) : : "r30", "r31");
}

void s_c (void)
{
  char a[33];
  TRASH_SOME_CR;
  USE_SOME_CR;
  __asm __volatile ("#%0" : "=m" (a) : : "cr2");
}

void s_0 (void)
{
  char a[33];
  __asm __volatile ("#%0" : "=m" (a) );
}

#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
void wb_all (void)
{
  char b[10];
  char *nb_all (void)
  {
    char a[33000];
    TRASH_ALL_CR;
    TRASH_ALL_VR;
    TRASH_ALL_FPR;
    TRASH_ALL_GPR;
    USE_ALL_CR;
    USE_ALL_VR;
    USE_ALL_FPR;
    USE_ALL_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "cr3", "cr4", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "fr14", "fr15", "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23", "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31", "r14", "r15", "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31");
    return b;
  }
  if (nb_all() != b)
    abort ();
}

void wb_cvfr (void)
{
  char b[10];
  char *nb_cvfr (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_VR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (nb_cvfr () != b)
    abort ();
}

void wb_vfr (void)
{
  char b[10];
  char *nb_vfr (void)
  {
    char a[33000];
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_VR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (nb_vfr () != b)
    abort ();
}

void wb_cvf (void)
{
  char b[10];
  char *nb_cvf (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    USE_SOME_CR;
    USE_SOME_VR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31", "fr28", "fr31");
    return b;
  }
  if (nb_cvf () != b)
    abort ();
}

void wb_vf (void)
{
  char b[10];
  char *nb_vf (void)
  {
    char a[33000];
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    USE_SOME_VR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31", "fr28", "fr31");
    return b;
  }
  if (nb_vf () != b)
    abort ();
}
#endif

void wb_cvr (void)
{
  char b[10];
  char *nb_cvr (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_VR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31", "r30", "r31");
    return b;
  }
  if (nb_cvr () != b)
    abort ();
}

void wb_vr (void)
{
  char b[10];
  char *nb_vr (void)
  {
    char a[33000];
    TRASH_SOME_VR;
    TRASH_SOME_GPR;
    USE_SOME_VR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31", "r30", "r31");
    return b;
  }
  if (nb_vr () != b)
    abort ();
}

void wb_cv (void)
{
  char b[10];
  char *nb_cv (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    USE_SOME_CR;
    USE_SOME_VR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31");
    return b;
  }
  if (nb_cv () != b)
    abort ();
}

void wb_v (void)
{
  char b[10];
  char *nb_v (void)
  {
    char a[33000];
    TRASH_SOME_VR;
    USE_SOME_VR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31");
    return b;
  }
  if (nb_v () != b)
    abort ();
}
#endif

#ifndef __NO_FPRS__
void wb_cfr (void)
{
  char b[10];
  char *nb_cfr (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (nb_cfr () != b)
    abort ();
}

void wb_fr (void)
{
  char b[10];
  char *nb_fr (void)
  {
    char a[33000];
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (nb_fr () != b)
    abort ();
}

void wb_cf (void)
{
  char b[10];
  char *nb_cf (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    TRASH_SOME_FPR;
    USE_SOME_CR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "fr28", "fr31");
    return b;
  }
  if (nb_cf () != b)
    abort ();
}

void wb_f (void)
{
  char b[10];
  char *nb_f (void)
  {
    char a[33000];
    TRASH_SOME_FPR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "fr28", "fr31");
    return b;
  }
  if (nb_f () != b)
    abort ();
}
#endif

void wb_cr (void)
{
  char b[10];
  char *nb_cr (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "r30", "r31");
    return b;
  }
  if (nb_cr () != b)
    abort ();
}

void wb_r (void)
{
  char b[10];
  char *nb_r (void)
  {
    char a[33000];
    TRASH_SOME_GPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "r30", "r31");
    return b;
  }
  if (nb_r () != b)
    abort ();
}

void wb_c (void)
{
  char b[10];
  char *nb_c (void)
  {
    char a[33000];
    TRASH_SOME_CR;
    USE_SOME_CR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2");
    return b;
  }
  if (nb_c () != b)
    abort ();
}

void wb_0 (void)
{
  char b[10];
  char *nb_0 (void)
  {
    char a[33000];
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) );
    return b;
  }
  if (nb_0 () != b)
    abort ();
}

#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
void ws_all (void)
{
  char b[10];
  char *ns_all (void)
  {
    char a[33];
    TRASH_ALL_CR;
    TRASH_ALL_VR;
    TRASH_ALL_FPR;
    TRASH_ALL_GPR;
    USE_ALL_CR;
    USE_ALL_VR;
    USE_ALL_FPR;
    USE_ALL_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "cr3", "cr4", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "fr14", "fr15", "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23", "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31", "r14", "r15", "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31");
    return b;
  }
  if (ns_all() != b)
    abort ();
}

void ws_cvfr (void)
{
  char b[10];
  char *ns_cvfr (void)
  {
    char a[33];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_VR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (ns_cvfr () != b)
    abort ();
}

void ws_vfr (void)
{
  char b[10];
  char *ns_vfr (void)
  {
    char a[33];
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_VR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31", "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (ns_vfr () != b)
    abort ();
}

void ws_cvf (void)
{
  char b[10];
  char *ns_cvf (void)
  {
    char a[33];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    USE_SOME_CR;
    USE_SOME_VR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31", "fr28", "fr31");
    return b;
  }
  if (ns_cvf () != b)
    abort ();
}

void ws_vf (void)
{
  char b[10];
  char *ns_vf (void)
  {
    char a[33];
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    USE_SOME_VR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31", "fr28", "fr31");
    return b;
  }
  if (ns_vf () != b)
    abort ();
}
#endif

void ws_cvr (void)
{
  char b[10];
  char *ns_cvr (void)
  {
    char a[33];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_VR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31", "r30", "r31");
    return b;
  }
  if (ns_cvr () != b)
    abort ();
}

void ws_vr (void)
{
  char b[10];
  char *ns_vr (void)
  {
    char a[33];
    TRASH_SOME_VR;
    TRASH_SOME_FPR;
    USE_SOME_VR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31", "r30", "r31");
    return b;
  }
  if (ns_vr () != b)
    abort ();
}

void ws_cv (void)
{
  char b[10];
  char *ns_cv (void)
  {
    char a[33];
    TRASH_SOME_CR;
    TRASH_SOME_VR;
    USE_SOME_CR;
    USE_SOME_VR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "v26", "v27", "v31");
    return b;
  }
  if (ns_cv () != b)
    abort ();
}

void ws_v (void)
{
  char b[10];
  char *ns_v (void)
  {
    char a[33];
    TRASH_SOME_VR;
    USE_SOME_VR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "v26", "v27", "v31");
    return b;
  }
  if (ns_v () != b)
    abort ();
}
#endif

#ifndef __NO_FPRS__
void ws_cfr (void)
{
  char b[10];
  char *ns_cfr (void)
  {
    char a[33];
    TRASH_SOME_CR;
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (ns_cfr () != b)
    abort ();
}

void ws_fr (void)
{
  char b[10];
  char *ns_fr (void)
  {
    char a[33];
    TRASH_SOME_FPR;
    TRASH_SOME_GPR;
    USE_SOME_FPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "fr28", "fr31", "r30", "r31");
    return b;
  }
  if (ns_fr () != b)
    abort ();
}

void ws_cf (void)
{
  char b[10];
  char *ns_cf (void)
  {
    char a[33];
    TRASH_SOME_CR;
    TRASH_SOME_FPR;
    USE_SOME_CR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "fr28", "fr31");
    return b;
  }
  if (ns_cf () != b)
    abort ();
}

void ws_f (void)
{
  char b[10];
  char *ns_f (void)
  {
    char a[33];
    TRASH_SOME_FPR;
    USE_SOME_FPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "fr28", "fr31");
    return b;
  }
  if (ns_f () != b)
    abort ();
}
#endif

void ws_cr (void)
{
  char b[10];
  char *ns_cr (void)
  {
    char a[33];
    TRASH_SOME_CR;
    TRASH_SOME_GPR;
    USE_SOME_CR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2", "r30", "r31");
    return b;
  }
  if (ns_cr () != b)
    abort ();
}

void ws_r (void)
{
  char b[10];
  char *ns_r (void)
  {
    char a[33];
    TRASH_SOME_GPR;
    USE_SOME_GPR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "r30", "r31");
    return b;
  }
  if (ns_r () != b)
    abort ();
}

void ws_c (void)
{
  char b[10];
  char *ns_c (void)
  {
    char a[33];
    TRASH_SOME_CR;
    USE_SOME_CR;
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) : : "cr2");
    return b;
  }
  if (ns_c () != b)
    abort ();
}

void ws_0 (void)
{
  char b[10];
  char *ns_0 (void)
  {
    char a[33];
    __asm __volatile ("#%0 %1" : "=m" (a), "=m" (b) );
    return b;
  }
  if (ns_0 () != b)
    abort ();
}

int main (void)
{
  INIT_REGS;
  USE_ALL_CR;
#ifdef __ALTIVEC__
  USE_ALL_VR;
#ifndef __NO_FPRS__
  USE_ALL_FPR;
#endif
#endif
  USE_ALL_GPR;
#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
  b_all ();
  VERIFY_REGS;
  b_cvfr ();
  VERIFY_REGS;
  b_vfr ();
  VERIFY_REGS;
  b_cvf ();
  VERIFY_REGS;
  b_vf ();
  VERIFY_REGS;
#endif
  b_cvr ();
  VERIFY_REGS;
  b_vr ();
  VERIFY_REGS;
  b_cv ();
  VERIFY_REGS;
  b_v ();
  VERIFY_REGS;
#endif
#ifndef __NO_FPRS__
  b_cfr ();
  VERIFY_REGS;
  b_fr ();
  VERIFY_REGS;
  b_cf ();
  VERIFY_REGS;
  b_f ();
  VERIFY_REGS;
#endif
  b_cr ();
  VERIFY_REGS;
  b_r ();
  VERIFY_REGS;
  b_c ();
  VERIFY_REGS;
  b_0 ();
  VERIFY_REGS;
#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
  s_all ();
  VERIFY_REGS;
  s_cvfr ();
  VERIFY_REGS;
  s_vfr ();
  VERIFY_REGS;
  s_cvf ();
  VERIFY_REGS;
  s_vf ();
  VERIFY_REGS;
#endif
  s_cvr ();
  VERIFY_REGS;
  s_vr ();
  VERIFY_REGS;
  s_cv ();
  VERIFY_REGS;
  s_v ();
  VERIFY_REGS;
#endif
#ifndef __NO_FPRS__
  s_cfr ();
  VERIFY_REGS;
  s_fr ();
  VERIFY_REGS;
  s_cf ();
  VERIFY_REGS;
  s_f ();
  VERIFY_REGS;
#endif
  s_cr ();
  VERIFY_REGS;
  s_r ();
  VERIFY_REGS;
  s_c ();
  VERIFY_REGS;
  s_0 ();
  VERIFY_REGS;
#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
  wb_all ();
  VERIFY_REGS;
  wb_cvfr ();
  VERIFY_REGS;
  wb_vfr ();
  VERIFY_REGS;
  wb_cvf ();
  VERIFY_REGS;
  wb_vf ();
  VERIFY_REGS;
#endif
  wb_cvr ();
  VERIFY_REGS;
  wb_vr ();
  VERIFY_REGS;
  wb_cv ();
  VERIFY_REGS;
  wb_v ();
  VERIFY_REGS;
#endif
#ifndef __NO_FPRS__
  wb_cfr ();
  VERIFY_REGS;
  wb_fr ();
  VERIFY_REGS;
  wb_cf ();
  VERIFY_REGS;
  wb_f ();
  VERIFY_REGS;
#endif
  wb_cr ();
  VERIFY_REGS;
  wb_r ();
  VERIFY_REGS;
  wb_c ();
  VERIFY_REGS;
  wb_0 ();
  VERIFY_REGS;
#ifdef __ALTIVEC__
#ifndef __NO_FPRS__
  ws_all ();
  VERIFY_REGS;
  ws_cvfr ();
  VERIFY_REGS;
  ws_vfr ();
  VERIFY_REGS;
  ws_cvf ();
  VERIFY_REGS;
  ws_vf ();
  VERIFY_REGS;
#endif
  ws_cvr ();
  VERIFY_REGS;
  ws_vr ();
  VERIFY_REGS;
  ws_cv ();
  VERIFY_REGS;
  ws_v ();
  VERIFY_REGS;
#endif
#ifndef __NO_FPRS__
  ws_cfr ();
  VERIFY_REGS;
  ws_fr ();
  VERIFY_REGS;
  ws_cf ();
  VERIFY_REGS;
  ws_f ();
  VERIFY_REGS;
#endif
  ws_cr ();
  VERIFY_REGS;
  ws_r ();
  VERIFY_REGS;
  ws_c ();
  VERIFY_REGS;
  ws_0 ();
  VERIFY_REGS;
  return 0;
}
