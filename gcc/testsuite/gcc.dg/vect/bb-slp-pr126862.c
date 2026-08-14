/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>
static uint16_t
(safe_mod_func_uint16_t_u_u)(uint16_t ui1, uint16_t ui2 ){
  return
    (ui2 == 0) ?
    ((ui1)) :
    (ui1 % ui2);
}
struct a {};
int32_t b[4][5];
int16_t c[5];
uint16_t d;
int32_t *e(struct a, uint16_t, uint32_t, int32_t *, int64_t);
uint32_t f() {
  int32_t g[4][5];
  for (b[3][3] = 0; b[3][3] <= 3; b[3][3]++) {
    struct a h;
    e(h, 0, g[2][4], &b[3][3], b[9][4]);
  }
}
int32_t *e(struct a, uint16_t, uint32_t, int32_t *k, int64_t) {
  int32_t i[4];
  uint16_t *j = &d;
  if (c[4] ^= safe_mod_func_uint16_t_u_u(++*j, 0), *i)
    *j = 0;
  return k;
}
