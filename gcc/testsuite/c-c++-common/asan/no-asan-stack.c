/* { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } && lp64 } } } */
/* { dg-options "--param asan-stack=0" } */
#include <string.h>

volatile int one = 1;

int
main ()
{
  volatile char a1[] = {(char)one, 2, 3, 4};
  volatile char a2[] = {1, (char)(2*one), 3, 4};
  volatile int res = memcmp ((void *)a1,(void *)a2, 5 + one);
  return 0;
}

/* { dg-final { scan-assembler-not "0x41b58ab3|0x41B58AB3|1102416563" } } */
