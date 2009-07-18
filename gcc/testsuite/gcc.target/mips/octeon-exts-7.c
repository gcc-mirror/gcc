/* Remove the redundant sign-extension after the sign-extraction.  */
/* { dg-do compile } */
/* { dg-options "-O -march=octeon -mgp64" } */
/* { dg-final { scan-assembler-times "\texts\t" 1 } } */
/* { dg-final { scan-assembler-not "sll|sra" } } */

struct bar
{
  long long a:18;
  long long b:14;
};

NOMIPS16 int
f1 (struct bar *s)
{
  return s->b;
}
