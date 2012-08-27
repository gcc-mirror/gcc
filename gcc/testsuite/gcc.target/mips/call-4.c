/* See through some simple data-flow.  */
/* { dg-options "-mrelax-pic-calls" } */
/* { dg-final { scan-assembler "\\.reloc\t1f,R_MIPS_JALR,g\n1:\tjalr\t" } } */

NOMIPS16 f (int i)
{
  while (i--)
    g ();
}
