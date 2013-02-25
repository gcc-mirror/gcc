/* { dg-options "-fpeephole2 -mgp32 (-mmicromips)" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

void MICROMIPS
foo (long long l1, long long *l2)
{
  *l2 = l1;
}

/* { dg-final { scan-assembler "\tswp\t\\\$4,0\\(\\\$6\\)" } }*/
