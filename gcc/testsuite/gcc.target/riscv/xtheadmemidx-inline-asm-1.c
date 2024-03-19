/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */
/* { dg-options "-march=rv64gc_xtheadmemidx -mabi=lp64" } */

/* XTheadMemIdx support is implemented such that reg+reg addressing mode
   loads/stores are preferred over standard loads/stores.
   If this order changed using inline assembly, the result will be invalid
   instructions.  This test serves the purpose of documenting this
   limitation until a solution is available.  */

void foo (void *p, unsigned long off, unsigned long val)
{
  unsigned long *tmp = (unsigned long*)(p + off);
  asm volatile ("sd	%1,%0" : "=m"(*tmp) : "r"(val));
}

void bar (void *p, unsigned long off, unsigned long val)
{
  unsigned long *tmp = (unsigned long*)(p + off);
  asm volatile ("th.srd	%1,%0" : "=m"(*tmp) : "r"(val));
}

/* { dg-final { scan-assembler "sd\t\[a-z\]\[0-9\]+,0\\(\[a-z\]\[0-9\]+\\)" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-not "sd\t\[a-z\]\[0-9\]+,\[a-z\]\[0-9\]+,\[a-z\]\[0-9\]+,0" { xfail *-*-* } } } */
/* { dg-final { scan-assembler "th\.srd\t\[a-z\]\[0-9\]+,\[a-z\]\[0-9\]+,\[a-z\]\[0-9\]+,0" } } */
/* { dg-final { scan-assembler-not "th\.srd\t\[a-z\]\[0-9\]+,0\\(\[a-z\]\[0-9\]+\\)" } } */
