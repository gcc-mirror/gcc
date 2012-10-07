/* { dg-options "-mdspr2 -mgp32 -mtune=74kc" } */
/* References to RESULT within the loop need to have a higher frequency than
   references to RESULT outside the loop, otherwise there is no reason
   to prefer multiply/accumulator registers over GPRs.  */
/* { dg-skip-if "requires register frequencies" { *-*-* } { "-O0" "-Os" } { "" } } */

/* Check that the zero-initialization of the accumulator feeding into
   the madd is done by means of a mult instruction instead of mthi/mtlo.  */

NOMIPS16 long long f (int n, int *v, int m)
{
  long long result = 0;
  int i;

  for (i = 0; i < n; i++)
    result = __builtin_mips_madd (result, v[i], m);
  return result;
}

/* { dg-final { scan-assembler "\tmult\t\\\$ac.,\\\$0,\\\$0" } } */
/* { dg-final { scan-assembler-not "mthi\t" } } */
/* { dg-final { scan-assembler-not "mtlo\t" } } */
