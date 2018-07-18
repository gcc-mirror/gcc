/* See through some simple data-flow.  */
/* { dg-options "-mmicromips -mrelax-pic-calls" } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MICROMIPS_JALR,g\n1:\tjalrs?\t" } } */

extern void g (void);

int
NOMIPS16 f (int i)
{
  while (i--)
    g ();
}
