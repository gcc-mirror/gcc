/* { dg-do compile } */
/* { dg-additional-options "-march=gfx908 -O1" } */
/* { dg-final { scan-assembler "accvgpr" } } */

#ifndef TYPE
#define TYPE int
#endif

TYPE a[50];

int f()
{
  __asm__ volatile ("; fake <- %0" : "=v"(a[0]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[1]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[2]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[3]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[4]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[5]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[6]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[7]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[8]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[9]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[10]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[11]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[12]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[13]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[14]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[15]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[16]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[17]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[18]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[19]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[20]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[21]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[22]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[23]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[24]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[25]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[26]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[27]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[28]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[29]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[30]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[31]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[32]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[33]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[34]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[35]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[36]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[37]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[38]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[39]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[40]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[41]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[42]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[43]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[44]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[45]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[46]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[47]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[48]));
  __asm__ volatile ("; fake <- %0" : "=v"(a[49]));

  __asm__ volatile ("; fake -> %0" :: "v"(a[0]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[1]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[2]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[3]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[4]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[5]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[6]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[7]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[8]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[9]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[10]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[11]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[12]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[13]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[14]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[15]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[16]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[17]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[18]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[19]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[20]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[21]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[22]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[23]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[24]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[25]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[26]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[27]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[28]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[29]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[30]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[31]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[32]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[33]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[34]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[35]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[36]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[37]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[38]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[39]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[40]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[41]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[42]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[43]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[44]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[45]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[46]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[47]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[48]));
  __asm__ volatile ("; fake -> %0" :: "v"(a[49]));
}
