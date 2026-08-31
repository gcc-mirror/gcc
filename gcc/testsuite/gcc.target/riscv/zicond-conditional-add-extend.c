/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-O2 -march=rv64gc_zicond -mabi=lp64d -mbranch-cost=4" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" "-funroll-loops" } } */

unsigned int
xz_loop (unsigned int pos, unsigned int cur_match, unsigned int depth,
	 unsigned int cyclic_pos, unsigned int cyclic_size,
	 const unsigned int *son, const unsigned char *cur)
{
  unsigned int sum = 0;
  while (depth-- != 0)
    {
      unsigned int delta = pos - cur_match;
      const unsigned char *pb = cur - delta;
      cur_match = son[cyclic_pos - delta
		      + (delta > cyclic_pos ? cyclic_size : 0)];
      sum += cur_match + pb[0] + cur[0];
    }
  return sum;
}

/* { dg-final { scan-assembler-times {\tczero\.(eqz|nez)\t} 1 } } */
/* { dg-final { scan-assembler-not {\tbgeu\t} } } */
