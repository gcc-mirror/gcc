/* { dg-do compile } */
/* { dg-options "(HAS_MADD) -mgp32 -mtune=4kc" } */
/* References to X within the loop need to have a higher frequency than
   references to X outside the loop, otherwise there is no reason
   to prefer multiply/accumulator registers over GPRs.  */
/* { dg-skip-if "requires register frequencies" { *-*-* } { "-O0" "-Os" } { "" } } */
/* { dg-final { scan-assembler-not "\tmul\t" } } */
/* { dg-final { scan-assembler-not "\tmthi" } } */
/* { dg-final { scan-assembler-not "\tmtlo" } } */
/* { dg-final { scan-assembler "\tmult\t" } } */
/* { dg-final { scan-assembler "\tmadd\t" } } */

NOMIPS16 long long
f1 (int *a, int *b, int n)
{
  long long int x;
  int i;

  x = 0;
  for (i = 0; i < n; i++)
    x += (long long) a[i] * b[i];
  return x;
}
