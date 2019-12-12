/* { dg-options "-O2" } */

/* PR85160 */

/* Originally, the "x >> 14" are CSEd away (eventually becoming a srawi
   instruction), and the two ANDs remain separate instructions because
   combine cannot deal with this.

   Now that combine knows how to combine two RTL insns into two, it manages
   to make this just the sum of two rlwinm instructions.  */

int f(int x)
{
  return ((x >> 14) & 6) + ((x >> 14) & 4);
}

/* { dg-final { scan-assembler-not {\msrawi\M} } } */
