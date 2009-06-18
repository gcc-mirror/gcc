/* { dg-do compile { target arm*-*-* } } */
/* { dg-options "-mfp16-format=ieee" } */

/* Functions cannot have parameters of type __fp16.  */
extern void f (__fp16);		/* { dg-error "parameters cannot have __fp16 type" } */
extern void (*pf) (__fp16);	/* { dg-error "parameters cannot have __fp16 type" } */

/* These should be OK.  */
extern void g (__fp16 *);
extern void (*pg) (__fp16 *);
