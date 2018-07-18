/* { dg-do compile } */
/* { dg-options "-O3 -fno-vect-cost-model" } */

/* Check that we vectorize to a full 128-bit vector for _Float16 and __fp16
   types.  */

/* Enable ARMv8.2-A+fp16 so we have access to the vector instructions.  */
#pragma GCC target ("arch=armv8.2-a+fp16")

_Float16
sum_Float16 (_Float16 *__restrict__ __attribute__ ((__aligned__ (16))) a,
	     _Float16 *__restrict__ __attribute__ ((__aligned__ (16))) b,
	     _Float16 *__restrict__ __attribute__ ((__aligned__ (16))) c)
{
  for (int i = 0; i < 256; i++)
    a[i] = b[i] + c[i];
}

_Float16
sum_fp16 (__fp16 *__restrict__ __attribute__ ((__aligned__ (16))) a,
	  __fp16 *__restrict__ __attribute__ ((__aligned__ (16))) b,
	  __fp16 *__restrict__ __attribute__ ((__aligned__ (16))) c)
{
  for (int i = 0; i < 256; i++)
    a[i] = b[i] + c[i];
}

/* Two FADD operations on "8h" data widths, one from sum_Float16, one from
   sum_fp16.  */
/* { dg-final { scan-assembler-times "fadd\tv\[0-9\]\+.8h" 2 } } */
