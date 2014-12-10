/* See through some simple data-flow.  */
/* { dg-options "-mrelax-pic-calls" } */
/* { dg-final { scan-assembler-times "\\.reloc\t1f,R_MIPS_JALR,g\n1:\tjalrs?\t" 2 } } */

extern void g (void);

int
NOMIPS16 f ()
{
  g ();
  g ();
  return 1;
}
