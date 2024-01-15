/* { dg-options "isa_rev>=6 -mgp64 -mno-abicalls" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tsb\t" } } */
/* { dg-final { scan-assembler-not "\tsh\t" } } */
/* { dg-final { scan-assembler-not "\tlb\t" } } */
/* { dg-final { scan-assembler-not "\tlh\t" } } */
/* { dg-final { scan-assembler-not "\tswl\t" } } */
/* { dg-final { scan-assembler-not "\tswr\t" } } */
/* { dg-final { scan-assembler-not "\tlwl\t" } } */
/* { dg-final { scan-assembler-not "\tlwr\t" } } */
/* { dg-final { scan-assembler-not "\tsdl\t" } } */
/* { dg-final { scan-assembler-not "\tsdr\t" } } */
/* { dg-final { scan-assembler-not "\tldl\t" } } */
/* { dg-final { scan-assembler-not "\tldr\t" } } */
/* { dg-final { scan-assembler-times "\tsw\t" 1 } } */
/* { dg-final { scan-assembler-times "\tlw\t" 1 } } */
/* { dg-final { scan-assembler-times "\tsd\t" 1 } } */
/* { dg-final { scan-assembler-times "\tld\t" 1 } } */
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
