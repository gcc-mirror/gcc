/* { dg-do compile } */
/* { dg-options "-O2" } */

extern unsigned short var_5;
extern int var_8, var_9;
extern short arr_7[];
void test() {
  for (; 0 < (char)var_5;)
    for (int a(var_9 ? var_5 : 0); a < 3002972621U + 1291994699;
         a += 19499 - 19497)
      for (long b(var_8); b; b += 4)
        arr_7[a * b] = 0;
}
