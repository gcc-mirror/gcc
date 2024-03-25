/* { dg-do compile } */
/* { dg-additional-options "-march=gfx90a -O1" } */
/* { dg-final { scan-assembler {load[^\n]*a[0-9[]} } } */
/* { dg-final { scan-assembler {store[^\n]*a[0-9[]} } } */

#ifndef TYPE
#define TYPE int
#endif

TYPE a[50];

int f()
{
  __asm__ volatile ("; fake -> %0" :: "va"(a[0]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[1]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[2]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[3]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[4]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[5]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[6]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[7]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[8]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[9]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[10]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[11]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[12]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[13]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[14]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[15]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[16]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[17]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[18]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[19]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[20]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[21]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[22]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[23]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[24]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[25]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[26]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[27]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[28]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[29]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[30]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[31]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[32]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[33]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[34]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[35]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[36]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[37]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[38]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[39]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[40]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[41]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[42]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[43]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[44]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[45]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[46]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[47]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[48]));
  __asm__ volatile ("; fake -> %0" :: "va"(a[49]));

  __asm__ volatile ("; fake <- %0" : "+va"(a[0]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[1]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[2]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[3]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[4]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[5]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[6]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[7]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[8]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[9]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[10]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[11]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[12]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[13]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[14]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[15]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[16]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[17]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[18]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[19]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[20]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[21]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[22]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[23]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[24]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[25]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[26]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[27]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[28]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[29]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[30]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[31]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[32]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[33]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[34]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[35]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[36]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[37]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[38]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[39]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[40]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[41]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[42]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[43]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[44]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[45]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[46]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[47]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[48]));
  __asm__ volatile ("; fake <- %0" : "+va"(a[49]));
}
