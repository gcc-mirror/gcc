/* See through some simple data-flow.  */
/* { dg-options "-mno-micromips -mrelax-pic-calls" } */
/* { dg-final { scan-assembler-times "\\.reloc\t1f,R_MIPS_JALR,g\n1:\tjalrc?\t" 2 } } */

extern void g (void);

int
NOMIPS16 f ()
{
  g ();
  g ();
  return 1;
}
