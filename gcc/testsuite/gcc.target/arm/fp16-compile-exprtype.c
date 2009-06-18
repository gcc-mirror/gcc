/* Test that expressions involving __fp16 values have the right types.  */
/* { dg-do compile } */
/* { dg-options "-mfp16-format=ieee" } */

/* This produces a diagnostic if EXPR doesn't have type TYPE.  */
#define CHECK(expr,type)			\
  do {						\
    type v;					\
    __typeof (expr) *p = &v;			\
  } while (0);

volatile __fp16 f1;
volatile __fp16 f2;

int
main (void)
{
  CHECK (f1, __fp16);
  CHECK (+f1, float);
  CHECK (-f1, float);
  CHECK (f1+f2, float);
  CHECK ((__fp16)(f1+f2), __fp16);
  CHECK ((__fp16)99.99, __fp16);
  CHECK ((f1+f2, f1), __fp16);
}




