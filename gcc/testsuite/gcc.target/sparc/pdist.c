/* { dg-do compile } */
/* { dg-options "-mcpu=ultrasparc -mvis" } */
typedef long long int64_t;
typedef unsigned char vec8 __attribute__((vector_size(8)));

int64_t foo (vec8 a, vec8 b) {
  int64_t d = 0;
  d = __builtin_vis_pdist (a, b, d);
  return d;
}

int64_t bar (vec8 a, vec8 b) {
  int64_t d = 0;
  return __builtin_vis_pdist (a, b, d);
}

int64_t baz (vec8 a, vec8 b, int64_t d) {
  int64_t e = __builtin_vis_pdist (a, b, d);
  return e + d;
}

/* { dg-final { scan-assembler-times "pdist\t%" 3 } } */
