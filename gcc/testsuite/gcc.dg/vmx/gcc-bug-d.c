/* { dg-do compile } */
#include <altivec.h>
static void f() {}
extern void g() {}
extern vector unsigned char permute_128(vector unsigned char);

void foo()
{
  vector unsigned char input
    = {0,1,2,4,8,16,32,64,128,0,1,2,4,8,16,32};
  vector unsigned char result = permute_128(input);
  void (*p)() = f;
  void (*q)() = g;
}
