/* { dg-do run } */
/* { dg-options "-w" } */

#include <stdlib.h>

#ifndef __FLASH
#define __flash /* empty */
#endif

__extension__ typedef __uint24 uint24_t;
__extension__ typedef __int24 int24_t;

const __flash int24_t vals[] =
  {
    0, 1, 2, 3, -1, -2, -3, 0xff, 0x100, 0x101,
    0xffL * 0xff, 0xfffL * 0xfff, 0x101010L, 0xaaaaaaL
  };

void test_u (void)
{
  unsigned int i;
  unsigned long la, lb, lc;
  uint24_t a, b, c;

  int S = sizeof (vals) / sizeof (*vals);

  for (i = 0; i < 500; i++)
    {
      if (i < S*S)
        {
          a = vals[i / S];
          b = vals[i % S];
        }
      else
        {
          if (i & 1)
            a += 0x7654321L;
          else
            b += 0x5fe453L;
        }

      c = a * b;

      la = a;
      lb = b;
      lc = 0xffffff & (la * lb);
      
      if (c != lc)
        abort();
    }
}

#define TEST_N_U(A1,A2,B)                       \
  do {                                          \
    if ((0xffffff & (A1*B)) != A2*B)            \
      abort();                                  \
  } while (0)

void test_nu (void)
{
  unsigned long la;
  unsigned int i;
  int S = sizeof (vals) / sizeof (*vals);
  uint24_t a;
  
  for (i = 0; i < 500; i++)
    {
      a = i < S
        ? vals[i % S]
        : a + 0x7654321;

      la = a;

      TEST_N_U (la, a, 2);
      TEST_N_U (la, a, 3);
      TEST_N_U (la, a, 4);
      TEST_N_U (la, a, 5);
      TEST_N_U (la, a, 15);
      TEST_N_U (la, a, 16);
      TEST_N_U (la, a, 128);
      TEST_N_U (la, a, 0x1000);
    }
}
     
int main (void)
{
  test_u();
  test_nu();
  
  exit(0);
    
  return 0;
}
