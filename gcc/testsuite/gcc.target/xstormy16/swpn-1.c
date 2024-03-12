/* { dg-do compile } */
/* { dg-options "-O2" } */
unsigned char ior_1(unsigned char x) { return (x>>4) | (x<<4); }
unsigned char ior_2(unsigned char x) { return (x<<4) | (x>>4); }
unsigned char xor_1(unsigned char x) { return (x>>4) ^ (x<<4); }
unsigned char xor_2(unsigned char x) { return (x<<4) ^ (x>>4); }
unsigned char sum_1(unsigned char x) { return (x>>4) + (x<<4); }
unsigned char sum_2(unsigned char x) { return (x<<4) + (x>>4); }
/* { dg-final { scan-assembler-times "swpn r2" 6 } } */

