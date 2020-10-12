/* { dg-options "" } */

#include <arm_neon.h>

struct aegis128_state {
 uint8x16_t v[5];
};

void foo(const void *key, const void *iv, const void *const0, const void *const1)
{
 uint8x16_t k = vld1q_u8(key);
 uint8x16_t kiv = k ^ vld1q_u8(iv);
 struct aegis128_state st = {{
  kiv,
  vld1q_u8(const1),
  vld1q_u8(const0),
  k ^ vld1q_u8(const0),
  k ^ vld1q_u8(const1),
 }};
}
