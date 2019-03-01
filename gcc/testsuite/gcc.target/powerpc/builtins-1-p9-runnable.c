/* { dg-do run { target { powerpc*-*-linux* && { lp64 && p9vector_hw } } } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

#include <altivec.h>

void abort (void);

int main() {
  int i;
  vector float vfa, vfb;
  vector unsigned short vur, vuexpt;

  vfa = (vector float){3.4, 5.0, 20.0, 50.9 };
  vfb = (vector float){10.0, 40.0, 70.0, 100.0 };
  vuexpt = (vector unsigned short){ 3, 5, 20, 50,
                                    10, 40, 70, 100};

  vur = vec_pack_to_short_fp32 (vfa, vfb);

  for(i = 0; i< 8; i++) {
    if (vur[i] != vuexpt[i])
      abort();
  }
}
