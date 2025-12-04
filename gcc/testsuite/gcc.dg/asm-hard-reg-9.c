/* { dg-do compile { target { s390*-*-* || { x86_64-*-* && lp64 } } } } */
/* { dg-options "-O2" } */

/* Ensure that if the reload register for operand 2 is resued for operand 3,
   that exclude start hard regs coming from operand 3 are taken into account.
   Otherwise a different register than r8 may be chosen rendering the insn
   after LRA unsatisfiable.  */

long
test ()
{
  long x;
  __asm__ ("" : "=r" (x) : "0" (1000), "r" (0l), "{r8}" (0l));
  return x;
}
