/* { dg-do compile } */
/* { dg-options "-O2 -march=octeon" } */
/* { dg-final { scan-assembler-not "xor" } } */

unsigned
m (unsigned e);

NOMIPS16 void
f (unsigned i)
{
  unsigned j = m (i);
  h (j, i != j);
}
