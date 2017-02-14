/* { dg-options "-mmicromips -mrelax-pic-calls -mno-shared" } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MICROMIPS_JALR,g\n1:\tjalrs?\t" } } */
/* { dg-require-visibility "" } */

__attribute__ ((visibility ("hidden"))) void g ();

int
NOMIPS16 f ()
{
  g ();
  return 1;
}
