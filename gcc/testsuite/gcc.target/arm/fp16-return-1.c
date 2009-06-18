/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

/* Functions cannot return type __fp16.  */
extern __fp16 f (void);		/* { dg-error "cannot return __fp16" } */
extern __fp16 (*pf) (void);	/* { dg-error "cannot return __fp16" } */

/* These should be OK.  */
extern __fp16 *g (void);
extern __fp16 *(*pg) (void);
