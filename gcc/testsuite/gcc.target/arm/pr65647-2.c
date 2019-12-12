/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v6_arm_ok } */
/* { dg-options "-O3 -marm -march=armv6 -std=c99" } */

typedef struct {
  int i;
} x264_union32_t;
typedef struct {
  int level_idx;
} trellis_node_t;
int a, c, d, f, h, i = (int)&c;
trellis_node_t b[1][1];
short *e = 0;
short g;
void fn1() {
  int k[64 * 8 * 2];
  trellis_node_t *l = b[0];
  for (; i >= d; i--) {
    if (e[i]) {
      for (int j = 1; j < 8; j++) {
        ((x264_union32_t *)&k[a])->i = l[j].level_idx;
        l[j].level_idx = a;
        a++;
      }
      continue;
    }
    for (int j;; j++)
      ;
  }
  int m[6] __attribute__((aligned(16)));
  for (; h; h++, f++)
    g = m[h];
}
