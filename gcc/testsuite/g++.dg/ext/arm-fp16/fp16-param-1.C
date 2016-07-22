/* { dg-do compile { target arm*-*-* } } */
/* { dg-options "-mfp16-format=ieee" } */

/* Test that the ACLE macro is defined.  */
#if __ARM_FP16_ARGS != 1
#error Unexpected value for __ARM_FP16_ARGS
#endif

/* Test that __fp16 is supported as a parameter type.  */
extern void f (__fp16);
extern void (*pf) (__fp16);

extern void g (__fp16 *);
extern void (*pg) (__fp16 *);
