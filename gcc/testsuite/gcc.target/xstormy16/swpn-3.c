/* { dg-do compile } */
/* { dg-options "-O2" } */

short ior_1(unsigned short x) {
  return (signed char)(((x&0xf0)>>4) | ((x&0x0f)<<4));
}

short xor_1(unsigned short x) {
  return (signed char)(((x&0xf0)>>4) ^ ((x&0x0f)<<4));
}

short sum_1(unsigned short x) {
  return (signed char)(((x&0xf0)>>4) + ((x&0x0f)<<4));
}

short ior_2(unsigned short x) {
  return (signed char)(((x&0x0f)<<4) | ((x&0xf0)>>4));
}

short xor_2(unsigned short x) {
  return (signed char)(((x&0x0f)<<4) ^ ((x&0xf0)>>4));
}

short sum_2(unsigned short x) {
  return (signed char)(((x&0x0f)<<4) + ((x&0xf0)>>4));
}

/* { dg-final { scan-assembler-times "cbw" 6 } } */
