/* { dg-do compile } */
/* { dg-options "-c -O2 -ftree-vectorize -fdump-tree-ifcvt-stats" { target *-*-* } } */

typedef float afloat __attribute__ ((__aligned__(16)));
void foo(const afloat * __restrict__ zr_in,
		const afloat * __restrict__ zi_in,
		const afloat * __restrict__ zk_in,
		afloat * __restrict__ zr_out,
		afloat * __restrict__ zi_out,
		afloat * __restrict__ zk_out,
		float cr, float ci)
{
  unsigned int pi;
  float tmp_r, tmp_i, tmp_k;
  for (pi = 0; pi < (512)*(512); pi++) {
    float zr = zr_in[pi];
    float zi = zi_in[pi];
    float zk = zk_in[pi];
    
    if (zr*zr + zi*zi >= 4.0f) {
      tmp_r = zr;
      tmp_i = zi;
      tmp_k = zk;
    } else {
      tmp_r = (zr*zr - zi*zi + cr);
      tmp_i = (2 * zr * zi + ci);
      tmp_k = zk + 1.0f;
    }
    zr_out[pi] = tmp_r;
    zi_out[pi] = tmp_i;
    zk_out[pi] = tmp_k;
  }
}


/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */

