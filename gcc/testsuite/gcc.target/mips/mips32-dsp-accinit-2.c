/* { dg-options "-mdspr2 -mgp32 -mtune=4kp" } */
/* References to RESULT within the loop need to have a higher frequency than
   references to RESULT outside the loop, otherwise there is no reason
   to prefer multiply/accumulator registers over GPRs.  */
/* { dg-skip-if "requires register frequencies" { *-*-* } { "-O0" "-Os" } { "" } } */

/* Check that the zero-initialization of the accumulator feeding into
   the madd is done by means of an mthi & mtlo pair instead of a
   "mult $0,$0" instruction.  */

NOMIPS16 long long f (int n, int *v, int m)
{
  long long result = 0;
  int i;

  for (i = 0; i < n; i++)
    result = __builtin_mips_madd (result, v[i], m);
  return result;
}

/* { dg-final { scan-assembler-not "mult\t\[^\n\]*\\\$0" } } */
/* { dg-final { scan-assembler "\tmthi\t" } } */
/* { dg-final { scan-assembler "\tmtlo\t" } } */
