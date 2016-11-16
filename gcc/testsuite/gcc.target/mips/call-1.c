/* { dg-options "-mno-micromips -mrelax-pic-calls -mshared -foptimize-sibling-calls -mabi=32" } */
/* { dg-skip-if "requires -foptimize-sibling-calls" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,normal\n1:\tjalrc?\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,normal2\n1:\tjalrc?\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,staticfunc\n1:\tjalrc?\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail\n1:\tjrc?\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail2\n1:\tjrc?\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail3\n1:\tjrc?\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail4\n1:\tjrc?\t" } } */

__attribute__ ((noinline)) static void staticfunc () { asm (""); }
int normal ();
void normal2 ();

int
NOMIPS16 f (int *p)
{
  *p = normal ();
  normal2 ();
  staticfunc ();
  return 1;
}

int tail ();

int
NOMIPS16 h ()
{
  return tail ();
}

void tail2 ();

NOMIPS16 void g ()
{
  tail2 ();
}

__attribute__ ((visibility ("hidden"))) void tail3 ();

NOMIPS16 void j ()
{
  tail3 ();
}

__attribute__ ((noinline)) static void tail4 () { asm (""); }

NOMIPS16 void k ()
{
  tail4 ();
}
