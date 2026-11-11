/* { dg-do compile } */
/* { dg-options "-O2 -mbranch-protection=none" } */

/* The GCS stack switch reads and writes memory, so the second load of *P
   must not be CSEd with the first.  */

unsigned long
f (unsigned long *p)
{
  unsigned long a = *p;
  __builtin_aarch64_gcsss (p);
  return a ^ *p;
}

/* { dg-final { scan-assembler-times {\tldr\tx[0-9]+, \[x[0-9]+\]} 2 } } */
/* { dg-final { scan-assembler-not {\tmov\tw0, 0} } } */
