/* { dg-do compile } */
/* { dg-options "-mavx2 -O2 -fdump-rtl-cprop_hardreg-details" } */
/* { dg-final { scan-rtl-dump-not {replaced reg [0-9]* with [0-9]*} "cprop_hardreg" } } */

#include<stdint.h>
int test (uint8_t *p, uint32_t t[1][1], int n) {

  int sum = 0;
  uint32_t a0;
  for (int i = 0; i < 4; i++, p++)
    t[i][0] = p[0];

  for (int i = 0; i < 4; i++) {
    {
      int t0 = t[0][i] + t[0][i];
      a0 = t0;
    };
    sum += a0;
  }
  return (((uint16_t)sum) + ((uint32_t)sum >> 16)) >> 1;
}

