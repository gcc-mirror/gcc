/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-require-effective-target sqrt_insn } */
/* { dg-options "-O2 -fmath-errno -fdump-tree-cdce-details -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "cdce3.c:12: \[^\n\r]* function call is shrink-wrapped into error conditions\." "cdce" } } */
/* { dg-final { scan-tree-dump "sqrtf \\(\[^\n\r]*\\); \\\[tail call\\\]" "optimized" } } */
/* { dg-skip-if "doesn't have a sqrtf insn" { mmix-*-* } } */

float sqrtf (float);
float foo (float x)
{
  return sqrtf (x);
}

