/* { dg-do compile } */

int g_30, g_261, g_263, func_1___trans_tmp_17;
int **g_120;
int *g_530;
void func_1() {
  int *l_29 = &g_30;
  *l_29 = 1;
  g_263 = 0;
  for (; g_263 <= 1; g_263 += 1) {
    g_530 = 0;
    if (*l_29) {
      char *l_1694 = (char *)&g_261;
      *l_1694 &= **g_120;
    } else
      *l_29 ^= func_1___trans_tmp_17;
  }
}
