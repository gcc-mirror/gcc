/* { dg-do compile } */

#include <stdarg.h>

unsigned short in[40 +128];
int main (void) {
  int i = 0, j = 0;
  unsigned int diff;
  unsigned int s=0,sum=0;
  for (i = 0; i < 40;i++)
   {
     diff = 0;
     for (j = 0; j < 128;j+=8)
       diff += in[j+i];
     s += ((unsigned short)diff>>3);
   }
   if (s != sum)
     return -1;
   return 0;
}
