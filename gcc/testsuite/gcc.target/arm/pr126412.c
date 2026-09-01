/* PR rtl-optimization/126412 */
/* { dg-do run } */
/* { dg-require-effective-target arm_arch_v7a_arm_multilib } */
/* { dg-options "-O3 -fno-strict-aliasing" } */
/* { dg-add-options arm_arch_v7a_arm } */

int crc32_tab[256];
int crc32_context = -1, main_i, main_j;
long long g_9 = -9145554599436640560, g_13 = 5807806182820080170;
short g_44 = 6;
int *g_70;
static int **g_69 = &g_70;
int g_112[240] = {3};
char g_126;
static int g_170;
signed char g_253 = 10;
short **g_486;
static long long g_590 = 1;
static long long g_609 = 1;
long func_51___trans_tmp_28;
unsigned main_crc;
void crc32_byte(char b) {
  crc32_context =
      crc32_context >> 8 & 16777215 ^ crc32_tab[(crc32_context ^ b) & 255];
}
void crc32_8bytes(long long val) {
  crc32_context =
      crc32_context >> 8 & 16777215 ^ crc32_tab[(crc32_context ^ val) & 255];
  crc32_byte(val >> 8);
  crc32_byte(val >> 16);
  crc32_byte(val >> 24);
  crc32_byte(val >> 32);
  crc32_byte(val >> 40);
  crc32_byte(val >> 48);
  crc32_byte(val >> 56);
}
static int func_51() {
  long __trans_tmp_26;
  for (;;) {
    int *l_247_1_1_4 = &g_112[8];
    signed char *l_634 = &g_253;
    *g_69 = l_247_1_1_4;
    if (__trans_tmp_26)
      for (; g_170; g_170 -= 1) {
        if (g_253)
          g_126 = 0;
      }
    else if (func_51___trans_tmp_28) {
      *g_486 = 0;
      g_590 &= --g_609;
    }
    *l_634 |= 2;
    if ((short)(__INTPTR_TYPE__)*g_69)
      return 0;
  }
}
int main(void) {
  for (; main_i < 256; main_i++) {
    main_crc = main_i;
    main_j = 8;
    for (; main_j; main_j--)
      if (main_crc & 1)
        main_crc = main_crc >> 1 ^ 3988292384;
      else
        main_crc >>= 1;
    crc32_tab[main_i] = main_crc;
  }
  long long *l_8 = &g_9, *l_12 = &g_13;
  *l_12 ^= ++*l_8;
  g_44 = 0;
  for (; g_44 != 21; ++g_44)
    func_51();
  crc32_8bytes(0);
  crc32_8bytes(g_9);
  crc32_8bytes(g_13);
  if ((unsigned)(crc32_context ^ -1) != 0xe841b57e)
    __builtin_abort();
  return 0;
}
