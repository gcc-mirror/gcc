// { dg-do compile }
// { dg-additional-options "-O3" }

extern int arr_6[];
extern char arr_7[] __attribute__((aligned));
void test(short a, bool, int p8) {
  for (bool b = 0; b < (bool)p8; b = 1)
    for (short c = 0; c < 5; c++) {
      arr_6[c] = (long)2 << a - 30574;
      arr_7[c] = 0;
    }
}
