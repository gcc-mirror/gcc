/* { dg-do compile } */

int g_149, g_167, g_481;
int main() {
  int *l_1478 = &g_149;
  *l_1478 ^= g_167;
lbl_1481:
  for (;;) {
    g_481 = 1;
    for (; g_481 < 100000; g_481 += 1) {
      g_167 ^= *l_1478;
      if (g_149)
        goto lbl_1481;
    }
  }
}
