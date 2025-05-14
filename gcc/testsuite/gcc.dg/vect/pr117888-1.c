/* { dg-do compile } */
/* { dg-options "-O3 -funroll-loops -fdump-tree-vect-details" } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_shift } */
/* { dg-additional-options "-mavx2" { target x86_64-*-* i?86-*-* } } */
/* { dg-additional-options "--param max-completely-peeled-insns=200" { target powerpc64*-*-* } } */
/* { dg-additional-options "-mlsx" { target loongarch64-*-* } } */

typedef unsigned short ggml_fp16_t;
static float table_f32_f16[1 << 16];

inline static float ggml_lookup_fp16_to_fp32(ggml_fp16_t f) {
  unsigned short s;
  __builtin_memcpy(&s, &f, sizeof(unsigned short));
  return table_f32_f16[s];
}

typedef struct {
  ggml_fp16_t d;
  ggml_fp16_t m;
  unsigned char qh[4];
  unsigned char qs[32 / 2];
} block_q5_1;

typedef struct {
  float d;
  float s;
  char qs[32];
} block_q8_1;

void ggml_vec_dot_q5_1_q8_1(const int n, float * restrict s, const void * restrict vx, const void * restrict vy) {
  const int qk = 32;
  const int nb = n / qk;

  const block_q5_1 * restrict x = vx;
  const block_q8_1 * restrict y = vy;

  float sumf = 0.0;

  for (int i = 0; i < nb; i++) {
    unsigned qh;
    __builtin_memcpy(&qh, x[i].qh, sizeof(qh));

    int sumi = 0;

    if (qh) {
      for (int j = 0; j < qk/2; ++j) {
	const unsigned char xh_0 = ((qh >> (j + 0)) << 4) & 0x10;
	const unsigned char xh_1 = ((qh >> (j + 12)) ) & 0x10;

	const int x0 = (x[i].qs[j] & 0xF) | xh_0;
	const int x1 = (x[i].qs[j] >> 4) | xh_1;

	sumi += (x0 * y[i].qs[j]) + (x1 * y[i].qs[j + qk/2]);
      }
    }
    else {
      for (int j = 0; j < qk/2; ++j) {
	const int x0 = (x[i].qs[j] & 0xF);
	const int x1 = (x[i].qs[j] >>  4);

	sumi += (x0 * y[i].qs[j]) + (x1 * y[i].qs[j + qk/2]);
      }
    }

    sumf += (ggml_lookup_fp16_to_fp32(x[i].d)*y[i].d)*sumi + ggml_lookup_fp16_to_fp32(x[i].m)*y[i].s;
  }

  *s = sumf;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
