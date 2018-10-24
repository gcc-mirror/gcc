/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-alias" } */

int *abc(int** __restrict a, int** __restrict b)
{
  *a = 0;                     // clique 1 base 1
  asm volatile("":"+m"(*b));  // clique 0 base 0 (wrong)
  *a = 0;                     // clique 1 base 1
  return *b;                  // clique 1 base 2 (what it should be)
}

/* { dg-final { scan-tree-dump-times "clique 1 base \[12\]" 5 "optimized" } } */
/* On RTL we can DSE one of the stores of zero, on the GIMPLE level we
   do not bother to do disambiguation against asms.  */
/* { dg-final { scan-tree-dump-times " = 0;" 1 "optimized" { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times "mov\[^\n\r\]*0" 1 { target { x86_64-*-* i?86-*-* } } } } */
