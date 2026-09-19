/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32i_zilsd -mabi=ilp32" } */
/* { dg-additional-options "-mzilsd-strict-align" } */

/* Same split-argument reconstruction as zilsd-align-split-arg-2.c, but with
   a large stack frame so the stack offset of the spilled high word of n no
   longer fits in a single I-type immediate and address legitimization
   materializes it into a register before the access.  The middle-end still
   marks the reconstructed slot as only 4-byte aligned, so under
   -mzilsd-strict-align the backend must split the access into two 32-bit
   loads.  */

long long
f7 (long long *a, long long *b, long long *c, long long *d,
    long long *e, long long *f, long long *g, long long n)
{
  volatile char buf[4096];
  buf[0] = 1;
  return n;
}

/* The only doubleword access in this function is the misaligned access of
   n, so no zilsd ld/sd may appear at all; the access must use two lw.  */
/* { dg-final { scan-assembler-not {ld\t} } } */
/* { dg-final { scan-assembler-not {sd\t} } } */
