/* { dg-do run } */
/* { dg-additional-options "-O0 -std=gnu99" } */
/* We used to generate a separate riscv_read_vl () after the FoF load.
   In case of -O0 (or otherwise) it could happen that "g" wouldn't
   get a hard reg and we'd need to store it, clobbering VL.
   This leads to an infinite loop or a segfault.  */

#include <riscv_vector.h>

uint8_t a[1];
int16_t b[1];

int main ()
{
  for (size_t c = 0, avl = 1; avl > 0;)
    {
      size_t d = avl;
      vint16mf2_t g = __riscv_vle16ff_v_i16mf2 (&b[c], &d, d);
      avl -= d;
      c += d; // Segmentation fault
    }
}
