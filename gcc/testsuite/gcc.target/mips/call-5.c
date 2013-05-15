/* Like call-1.c, but for n32.  We cannot use sibling calls for tail and tail2
   in this case (PR target/57260).  */
/* { dg-options "-mrelax-pic-calls -mshared -foptimize-sibling-calls -mabi=n32" } */
/* { dg-skip-if "requires -foptimize-sibling-calls" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,normal\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,normal2\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,staticfunc\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail2\n1:\tjalr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail3\n1:\tjr\t" } } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,tail4\n1:\tjr\t" } } */

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
