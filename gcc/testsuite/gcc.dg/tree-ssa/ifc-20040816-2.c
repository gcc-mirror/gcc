/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize -fdump-tree-ifcvt-stats-blocks-details" { target *-*-* } } */

void foo(const int * __restrict__ zr_in,
		const int * __restrict__ zi_in,
		const int * __restrict__ zk_in,
		int * __restrict__ zr_out,
		int * __restrict__ zi_out,
		int * __restrict__ zk_out,
		int cr, int ci)
{
  unsigned int pi;
  int tmp_r, tmp_i, tmp_k;
#if __SIZEOF_INT__ >= 4
  for (pi = 0; pi < (512)*(512); pi++) {
#else
  for (pi = 0; pi < (32)*(32); pi++) {
#endif
    int zr = zr_in[pi];
    int zi = zi_in[pi];
    int zk = zk_in[pi];
    
    if (zr*zr + zi*zi >= 4) {
      tmp_r = zr;
      tmp_i = zi;
      tmp_k = zk;
    } else {
      tmp_r = (zr*zr - zi*zi + cr);
      tmp_i = (2 * zr * zi + ci);
      tmp_k = zk + 1;
    }
    zr_out[pi] = tmp_r;
    zi_out[pi] = tmp_i;
    zk_out[pi] = tmp_k;
  }
}


/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */

/* We insert into code
   if (LOOP_VECTORIZED (...))
   which is folded by vectorizer.  Both outgoing edges must have probability
   100% so the resulting profile match after folding.  */
/* { dg-final { scan-tree-dump-times "Invalid sum of outgoing probabilities 200.0" 1 "ifcvt" } } */
/* { dg-final { scan-tree-dump-times "Invalid sum of incoming counts" 1 "ifcvt" } } */
