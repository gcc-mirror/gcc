/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned short ior_1(unsigned short x) { return ((x&0xf0)>>4) | ((x&0x0f)<<4); }
unsigned short xor_1(unsigned short x) { return ((x&0xf0)>>4) ^ ((x&0x0f)<<4); }
unsigned short sum_1(unsigned short x) { return ((x&0xf0)>>4) + ((x&0x0f)<<4); }

unsigned short ior_2(unsigned short x) { return ((x&0x0f)<<4) | ((x&0xf0)>>4); }
unsigned short xor_2(unsigned short x) { return ((x&0x0f)<<4) ^ ((x&0xf0)>>4); }
unsigned short sum_2(unsigned short x) { return ((x&0x0f)<<4) + ((x&0xf0)>>4); }

/* { dg-final { scan-assembler-times "swpn r2" 6 } } */
/* { dg-final { scan-assembler-times "and r2,#255" 6 } } */

