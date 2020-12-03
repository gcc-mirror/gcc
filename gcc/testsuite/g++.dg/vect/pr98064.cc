// { dg-do compile }
// { dg-additional-options "-O3" }

const long long &min(const long long &__a, long long &__b) {
  if (__b < __a)
    return __b;
  return __a;
}
extern long var_2;
extern int var_3, var_8;
extern long long var_5;
extern unsigned short arr_353[];
extern short arr_362[];
extern int arr_518[];
void test() {
    for (char d = 0; d < 013; d += 4) {
        for (char e = 0; e < 11; e++)
            arr_353[e] = var_2 | min((long long)7, var_5);
        for (int f = var_5; f; f += 4)
            for (short g = var_8; g; g++)
                arr_362[g] = 0;
    }
    for (short h = 5; (short)var_2; h += 5)
        arr_518[h] = 0;
}
