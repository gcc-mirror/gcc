/* { dg-do compile } */
/* { dg-options "-O3 -fpic -msve-vector-bits=512 -fno-schedule-insns" } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls_native } */

typedef float v16si __attribute__ ((vector_size (64)));

__thread v16si tx;

v16si foo (v16si a, v16si b, v16si c)
{
  v16si y;

  /* There is nothing stopping the compiler from making the tls call before
     loading the input variables off the stack.  However, there appears to
     be no way in C of enforcing this.  Thankfully the compiler doesn't
     do this reordering.  */

  y = a + tx + b + c;

  return y + 7;
}

/* { dg-final { scan-assembler-times {\tstr\tz[0-9]+,} 3 } } */
