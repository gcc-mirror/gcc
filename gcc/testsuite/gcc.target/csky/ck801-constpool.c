/* { dg-do compile } */
/* { dg-skip-if  "test is specific to ck801"  { csky-*-* }  { "*" }  { "-mcpu=ck801" }  }  */
/* { dg-csky-options "-O1" } */

/* Make sure that constant pools are emitted by the compiler for ck801.
   If this is deferred to the assembler, the compiler will compute
   incorrect branch offsets.  */

void f (unsigned int *u, long long int *l, float *f, double *d)
{
  *u = 0xdeadbeef;
  *l = 0xcafef00dc0ffeeULL;
  *f = 3.14159F;
  *d = 2.718281828459;
}

/* { dg-final { scan-assembler-times "\\.long" 6 } } */
