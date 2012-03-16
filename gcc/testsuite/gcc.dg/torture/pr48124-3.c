/* { dg-do run } */

extern void abort (void);
struct S1
{
  int f0;
  int:1;
  int f3;
  int:1;
  int:0;
  int f6:1;
};
int g_13 = 1;
volatile struct S1 g_118 = {
    1
};

void __attribute__((noinline))
func_46 ()
{
  for (g_13 = 0; g_13 >= 0; g_13 -= 1)
    g_118.f6 = 0;
}

int
main ()
{
  func_46 ();
  if (g_13 != -1)
    abort ();
  return 0;
}
