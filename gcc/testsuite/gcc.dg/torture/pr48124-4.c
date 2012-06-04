/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

extern void abort (void);
struct S1 {
    unsigned f0, f1;
    unsigned short f2, f3;
    unsigned f4 : 16;
    unsigned f5, f6;
    volatile unsigned f7 : 28;
};
static struct S1 g_76;
static struct S1 g_245 = {0,0,0,0,0,0,0,1};
static signed char g_323 = 0x80;
static void func_1(void)
{
  g_245.f7 &= 1;
  for (g_323 = 0; g_323 <= -1; g_323 -= 2) {
      g_76 = g_76;
      g_76.f4 ^= 11;
  }
}
int main()
{
  func_1();
  if (g_323 != 0 || g_245.f7 != 1)
    abort ();
  return 0;
}
