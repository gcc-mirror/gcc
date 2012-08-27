/* { dg-do compile } */
/* { dg-options "-march=octeon -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* The pop instruction does not depend on the word value to be sign extended. */
/* { dg-final { scan-assembler-not "sll\t" } } */

long long f(long long i)
{
  return __builtin_popcount (i);
}

