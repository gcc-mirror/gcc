/* { dg-options "-mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\tlw\t" } } */
/* { dg-final { scan-assembler-not "\tsll\t" } } */
/* { dg-final { scan-assembler-not "\tld\t" } } */

struct s
{
  long long a;
  int b;
};

int
foo (struct s *x)
{
  return x->a;
}

