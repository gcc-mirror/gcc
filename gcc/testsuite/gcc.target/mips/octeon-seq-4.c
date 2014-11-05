/* { dg-do compile } */
/* { dg-options "-march=octeon" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\txor" } } */

unsigned
m (unsigned e);

extern void h ();

NOMIPS16 void
f (unsigned i)
{
  unsigned j = m (i);
  h (j, i != j);
}
