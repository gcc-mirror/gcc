/* PR optimization/10876 */
/* Contributed by Christian Ehrhardt */

/* Verify that the SPARC port doesn't emit
   (minus) (reg) (const_int) insns.  */

void f(void)
{
  unsigned int butterfly, block, offset;
  double *Z;

  for (block = 0; block < 512; block += 512) {
    double T1re, T2re;
    offset = butterfly + block;
    T1re += T2re;
    T2re = Z[offset] + T1re;
  }
}
