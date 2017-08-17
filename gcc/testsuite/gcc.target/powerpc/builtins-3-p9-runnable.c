/* { dg-do run { target { powerpc64*-*-* && { lp64 && p9vector_hw } } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2 -mupper-regs-di" } */

#include <altivec.h> // vector

void abort (void);

int main() {
   int i;
   vector float vfr, vfexpt;
   vector unsigned short vusha;

   /* 1.0, -2.0, 0.0, 8.5, 1.5, 0.5, 1.25, -0.25 */
   vusha = (vector unsigned short){0B011110000000000, 0B1100000000000000,
                                   0B000000000000000, 0B0100100001000000,
                                   0B011111000000000, 0B0011100000000000,
                                   0B011110100000000, 0B1011010000000000};
   
   vfexpt = (vector float){1.0, -2.0, 0.0, 8.5};
   vfr = vec_extract_fp_from_shorth(vusha);

   for (i=0; i<4; i++) {
      if (vfr[i] != vfexpt[i])
         abort();
   }

   vfexpt = (vector float){1.5, 0.5, 1.25, -0.25};
   vfr = vec_extract_fp_from_shortl(vusha);

   for (i=0; i<4; i++) {
      if (vfr[i] != vfexpt[i])
         abort();
   }
}
