/* { dg-options "isa_rev<=5 -mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-times "\tsdl\t" 1 } } */
/* { dg-final { scan-assembler-times "\tsdr\t" 1 } } */
/* { dg-final { scan-assembler-times "\tldl\t" 1 } } */
/* { dg-final { scan-assembler-times "\tldr\t" 1 } } */
/* { dg-final { scan-assembler-times "\tswl\t" 1 } } */
/* { dg-final { scan-assembler-times "\tswr\t" 1 } } */
/* { dg-final { scan-assembler-times "\tlwl\t" 1 } } */
/* { dg-final { scan-assembler-times "\tlwr\t" 1 } } */
/* { dg-final { scan-assembler-not "\tnop" } } */

/* Test to make sure we produce the unaligned load/store for
   both 64bit and 32bits sized accesses.  */

struct s
{
  char c;
  int i;
  long long l;
} __attribute__ ((packed)) s __attribute__((aligned(1) ));

NOMIPS16 void
sd (long long l)
{
  s.l = l;
}

NOMIPS16 long long
ld ()
{
  return s.l;
}

NOMIPS16 void
sw (int i)
{
  s.i = i;
}

NOMIPS16 int
lw ()
{
  return s.i;
}
