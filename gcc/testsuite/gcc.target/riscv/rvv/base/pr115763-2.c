/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zfh_zvfhmin -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model" } */

void test (_Float16 *dest, _Float16 bias) {
  dest[0] = bias;
  dest[1] = bias;
}

/* { dg-final { scan-assembler-times {fsh\s+fa[0-9]+,[0-9]+\(sp\)} 1 } } */
/* { dg-final { scan-assembler-not {vfmv\.v\.x\s+v[0-9]+,\s*fa[0-9]+} } } */
