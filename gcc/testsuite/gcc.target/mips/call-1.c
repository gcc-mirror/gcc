/* { dg-options "-O2 -mrelax-pic-calls -mshared" } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,normal\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,normal2\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,staticfunc\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail\n1:\tjr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail2\n1:\tjr\t" } } */

__attribute__ ((noinline)) static void staticfunc () { asm (""); }
int normal ();
void normal2 ();

NOMIPS16 f (int *p)
{
  *p = normal ();
  normal2 ();
  staticfunc ();
  return 1;
}

int tail ();

NOMIPS16 h ()
{
  return tail ();
}

void tail2 ();

NOMIPS16 void g ()
{
  tail2 ();
}
