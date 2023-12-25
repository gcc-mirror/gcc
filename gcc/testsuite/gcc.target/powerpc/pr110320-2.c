/* PR target/110320 */
/* { dg-require-effective-target powerpc_pcrel } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mno-pcrel -ffixed-r0 -ffixed-r11 -ffixed-r12" } */

/* Ensure we don't use r2 as a normal volatile register for the code below.
   The test case ensures all of the parameter registers r3 - r10 are used
   and needed after we compute the expression "x + y" which requires a
   temporary.  The -ffixed-r* options disallow using the other volatile
   registers r0, r11 and r12.  That only leaves RA to choose from the more
   expensive non-volatile registers for the temporary to be assigned to.  */

extern long bar (long, long, long, long, long, long, long, long *);

long
foo (long r3, long r4, long r5, long r6, long r7, long r8, long r9, long *r10)
{
  *r10 = r3 + r4;
  return bar (r3, r4, r5, r6, r7, r8, r9, r10);
}

/* { dg-final { scan-assembler-not {\madd 2,3,4\M} } } */
