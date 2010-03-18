/* { dg-do run } */

int l_5_5_2 = 4;
int g_3[1][1];

void func_1 (void)
{
  for (g_3[0][0] = 1; g_3[0][0] < 8; g_3[0][0] += 7) {
    int *l_6 = &g_3[0][0];
    *l_6 = l_5_5_2;
  }
}

int main (void)
{
  func_1 ();
  if (g_3[0][0] != 11)
      __builtin_abort ();
  return 0;
}
