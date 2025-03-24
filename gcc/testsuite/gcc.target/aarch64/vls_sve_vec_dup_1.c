/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -msve-vector-bits=128" } */

float fasten_main_etot_0;
void fasten_main() {
  for (int l = 0; l < 2;) {
    int phphb_nz;
    for (; l < 32; l++) {
      float dslv_e = l && phphb_nz;
      fasten_main_etot_0 += dslv_e;
    }
  }
}

/* { dg-final { scan-assembler-not {bfi\tw\[0-9\]+} } } */
