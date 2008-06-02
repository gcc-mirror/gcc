/* { dg-do run } */
#include <stdio.h>

#define __ATTR_PROGMEM__ __attribute__((__progmem__))

#define PROGMEM __ATTR_PROGMEM__
char PROGMEM a1 = 0x12; 
int PROGMEM a2 = 0x2345; 
long PROGMEM a3 = 0x12345678; 
int main(void)
{
  printf("Hello World\n");
  return 0;
}
