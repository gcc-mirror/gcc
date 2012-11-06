/* Remove the redundant sign-extension after the sign-extraction.  */
/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\texts\t" 1 } } */
/* { dg-final { scan-assembler-not "\td?(sll|sra)" } } */

struct bar
{
  long long a:18;
  long long b:24;
  long long c:22;
};

NOMIPS16 int
f1 (struct bar *s)
{
  return s->b;
}
