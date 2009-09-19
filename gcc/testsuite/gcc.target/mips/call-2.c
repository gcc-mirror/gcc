/* See through some simple data-flow.  */
/* { dg-options "-O2 -mrelax-pic-calls" } */
/* { dg-final { scan-assembler-times "\\.reloc\t1f,R_MIPS_JALR,g\n1:\tjalr\t" 3 } } */

NOMIPS16 f (int i)
{
  while (i--)
    g ();
}

NOMIPS16 ff ()
{
  g ();
  g ();
  return 1;
}
