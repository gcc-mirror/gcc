/* { dg-do compile } */
#include <altivec.h>

typedef struct n001 {
  signed char m1;
  vector float m2;
  int m3;
  vector signed short m4;
  signed char m5;
  vector unsigned short m6;
} n001;

 n001 _i = {-4, {-1.84e+09, -2.13e+09, 1.43e+09, 1.14e+09}, 428762253, {-24132, 25298, -27969, -10358, 24164, -5157, -18143, -6509}, 40, {0x8737, 0xd7cf, 0xb6a7, 0x948f, 0x790b, 0x9255, 0x872d, 0xe72c}};
