/* Check that we use the octeon3 pipeline description.  */
/* { dg-do compile } */
/* { dg-options "-fschedule-insns2 -fdump-rtl-sched2 -march=octeon3" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */

NOMIPS16 int f (int a, int b)
{
  return a / b;
}

/* { dg-final { scan-rtl-dump "octeon_mult\\*17" "sched2" } }  */
