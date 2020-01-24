/* { dg-do compile } */
/* { dg-options "-O3 -fpic -msve-vector-bits=256 -fno-schedule-insns" } */
/* { dg-require-effective-target fpic } */
/* { dg-require-effective-target tls_native } */

typedef float v8si __attribute__ ((vector_size (32)));

__thread v8si tx;

v8si foo (v8si a, v8si b, v8si c)
{
  v8si y;

  /* There is nothing stopping the compiler from making the tls call before
     loading the input variables off the stack.  However, there appears to
     be no way in C of enforcing this.  Thankfully the compiler doesn't
     do this reordering.  */

  y = a + tx + b + c;

  return y + 7;
}

/* { dg-final { scan-assembler-times {\tstr\tz[0-9]+,} 3 } } */
