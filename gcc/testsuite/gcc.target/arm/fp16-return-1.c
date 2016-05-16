/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

/* Test that __fp16 is supported as a return type.  */
extern __fp16 f (void);
extern __fp16 (*pf) (void);

extern __fp16 *g (void);
extern __fp16 *(*pg) (void);
