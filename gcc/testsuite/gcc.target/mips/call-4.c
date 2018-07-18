/* See through some simple data-flow.  */
/* { dg-options "-mno-micromips -mrelax-pic-calls" } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,g\n1:\tjalrc?\t" } } */

extern void g (void);

int
NOMIPS16 f (int i)
{
  while (i--)
    g ();
}
