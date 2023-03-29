/* { dg-do compile } */
/* { dg-options "-O0 -march=tigerlake" } */
/* { dg-require-ifunc "" } */

#include <stdint.h>

typedef union {
  uint8_t v __attribute__((aligned(256))) __attribute__ ((vector_size(64 * sizeof(uint8_t))));
  uint8_t i[64] __attribute__((aligned(256)));
} stress_vec_u8_64_t;

typedef struct {
 struct {
  stress_vec_u8_64_t s;
  stress_vec_u8_64_t o;
  stress_vec_u8_64_t mask1;
  stress_vec_u8_64_t mask2;
 } u8_64;
} stress_vec_data_t;

__attribute__((target_clones("arch=alderlake", "default"))) 
void
stress_vecshuf_u8_64(stress_vec_data_t *data)
{
  stress_vec_u8_64_t *__restrict s;
  stress_vec_u8_64_t *__restrict mask1;
  stress_vec_u8_64_t *__restrict mask2;
  register int i;

  s = &data->u8_64.s;
  mask1 = &data->u8_64.mask1;
  mask2 = &data->u8_64.mask2;

  for (i = 0; i < 256; i++) {	/* was i < 65536 */
      stress_vec_u8_64_t tmp;

      tmp.v = __builtin_shuffle(s->v, mask1->v);
      s->v = __builtin_shuffle(tmp.v, mask2->v);
  }
}
