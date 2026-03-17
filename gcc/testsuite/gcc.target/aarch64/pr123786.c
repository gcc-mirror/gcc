/* { dg-do run } */
/* { dg-options "-O3 -mtune=generic-armv9-a" } */

#include <arm_neon.h>

int g_36[3];
short g_s;
signed char g_82, g_179, g_x, g_101;

__attribute__((noipa))
void check(void)
{
  if (g_36[0] != 1)
    __builtin_abort ();
}

int main(void)
{
BS_LABEL_3:
  short l_65 = g_s;
  if (g_x) goto BS_LABEL_6;
  for (; g_101; g_101 = 5)
  {
  BS_LABEL_6:
  }
  for (; g_82 < 1; g_82++)
  {
    switch (vqadds_u32 (0, 0))
      {
        case 4: goto BS_LABEL_3;
        case 9: goto BS_LABEL_3;
      }
    short si1 = (l_65 &= 1) || (g_179 &= 0) != 6;
    g_36[0] = g_36[2] == 0 ? 1 : si1 / g_36[2];
  }

  check ();
}
