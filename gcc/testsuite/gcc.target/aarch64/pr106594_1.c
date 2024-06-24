/* { dg-options "-O2" } */

extern const int constellation_64qam[64];

void foo(int nbits,
         const char *p_src,
         int *p_dst) {

  while (nbits > 0U) {
    char first = *p_src++;

    char index1 = ((first & 0x3) << 4) | (first >> 4);

    *p_dst++ = constellation_64qam[index1];

    nbits--;
  }
}

/* { dg-final { scan-assembler {(?n)\tldr\t.*\[x[0-9]+, w[0-9]+, sxtw #?2\]} } } */
