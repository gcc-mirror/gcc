/* { dg-do run } */

#include <stdio.h>

#define PROGMEM __attribute__((__progmem__))

const char PROGMEM a1 = 0x12; 
const int PROGMEM a2 = 0x2345; 
const long PROGMEM a3 = 0x12345678; 

int main(void)
{
  printf ("Hello World\n");
  return 0;
}
