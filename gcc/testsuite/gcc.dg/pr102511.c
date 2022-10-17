// { dg-do run }
// { dg-options "-O3" }

char arr_15 [8];
__attribute__((noipa))
void test(signed char a, unsigned short b, unsigned long long c,
          unsigned short f) {
  for (int d = b - 8; d < b; d += 2)
    for (short e = 0; e < (unsigned short)((f ? 122 : 0) ^ (a ? c : 0)) - 64055;
         e += 3)
      arr_15[d] = 42;
}
int main() {
    test(37, 8, 12325048486467861044ULL, 45936);
    for (int i = 0; i < 8; ++i)
      {
        if (arr_15[i] != ((i&1) ? 0 : 42))
          __builtin_abort();
      }
  return 0;
}
