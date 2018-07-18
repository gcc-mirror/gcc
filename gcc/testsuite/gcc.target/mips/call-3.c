/* { dg-options "-mno-micromips -mrelax-pic-calls -mno-shared" } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,g\n1:\tjalrc?\t" } } */
/* { dg-require-visibility "" } */

__attribute__ ((visibility ("hidden"))) void g ();

int
NOMIPS16 f ()
{
  g ();
  return 1;
}
