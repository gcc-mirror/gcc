/* { dg-do compile } */
/* { dg-additional-options "-O3 -g" } */

static short func1 (short si1, short si2) {
  return (si1 - si2);
}
unsigned short g_72;
extern int g_100[];
short g_173;
void func_42(void)
{
  for (g_173 = 10; g_173 > 0; g_173 = func1 (g_173, 1))
    for (g_72 = 1; g_72 < 5; g_72++)
      g_100[g_72] &= 1;
}
