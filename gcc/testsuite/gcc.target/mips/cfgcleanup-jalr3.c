/* { dg-do compile } */
/* { dg-options "-mabicalls -fpic -mno-mips16 -mno-micromips -fno-inline -fipa-ra -mcompact-branches=never" } */
/* { dg-skip-if "needs codesize optimization" { *-*-* } { "-O0" "-O1" "-O2" "-O3" } { "" } } */

static int foo (void* p) { return 0; }

static int bar (void* p) { return 1; }

int
test (void* p)
{
  int res = !p ? foo(p) : bar(p);

  register int tmp __asm__("$t0") = -1;
  __asm__ (""::"r"(tmp));

  return res;
}

/* { dg-final { scan-assembler-not "\\\.reloc\t1f,R_MIPS_JALR,foo" } } */
/* { dg-final { scan-assembler-not "\\\.reloc\t1f,R_MIPS_JALR,bar" } } */
/* { dg-final { scan-assembler "\\.set\tnomacro\n\tjalr\t\\\$25" } } */
