/* Test visibility attribute.  */
/* { dg-do compile { target ia64*-*-linux* } } */
/* { dg-options "-O2 -fpic" } */
/* { dg-final { scan-assembler "\\.hidden.*variable_j" } } */
/* { dg-final { scan-assembler "\\.hidden.*variable_m" } } */
/* { dg-final { scan-assembler "\\.protected.*baz" } } */
/* { dg-final { scan-assembler "gprel.*variable_i" } } */
/* { dg-final { scan-assembler "gprel.*variable_j" } } */
/* { dg-final { scan-assembler "ltoff.*variable_k" } } */
/* { dg-final { scan-assembler "gprel.*variable_l" } } */
/* { dg-final { scan-assembler "gprel.*variable_m" } } */
/* { dg-final { scan-assembler "ltoff.*variable_n" } } */

static int variable_i __attribute__ ((used));
int variable_j __attribute__((visibility ("hidden")));
int variable_k;
struct A { char a[64]; };
static struct A variable_l __attribute__((used, section (".sbss")));
struct A variable_m __attribute__((visibility ("hidden"), section(".sbss")));
struct A variable_n __attribute__((section (".sbss")));

void foo (void)
{
  variable_i = 0;
  variable_j = 0;
  variable_k = 0;
}

void bar (void)
{
  variable_l.a[10] = 0;
  variable_m.a[10] = 0;
  variable_n.a[10] = 0;
}

void __attribute__((visibility ("protected"))) baz (void)
{
}
