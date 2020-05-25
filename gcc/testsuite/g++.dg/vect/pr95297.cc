// { dg-do compile }
// { dg-additional-options "-O3 -fvect-cost-model=dynamic" }

extern bool var_10;
extern int var_16;
extern short var_17;
extern long var_18;
extern int arr_3[][13];

int min(const int &a, const int &b)
{
  return a < b ? a : b;
}

void test() {
    for (short a = 0; a < 010; a++)
      for (char b = 0; b < 012; b++)
	arr_3[a][b] = min(-var_10, 0) + 2147483647 >> var_10;
    var_16 = (bool)4;
    var_17 = 0;
    var_18 = -1594153176;
}
