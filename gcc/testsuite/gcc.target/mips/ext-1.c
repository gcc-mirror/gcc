/* { dg-do compile } */
/* { dg-options "isa_rev>=2 -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tdext\t" } } */
/* { dg-final { scan-assembler-not "\tand" } } */

struct
{
  unsigned long long a:9;
  unsigned long long d:35;
  unsigned long long e:10;
  unsigned long long f:10;
} t;

NOMIPS16 unsigned long long
f (void)
{
   return t.d;
}
