/* { dg-do compile } */

extern short var_0;
extern int var_3;
extern int arr_277[];
int a(int b, int c) { return b < c ? b : c; }
int e;
void test()
{
  e = var_0;
  for (int d = 0; d < 9; d++)
    if (var_3)
      arr_277[d] = a(var_0, -var_0);
}
