/* { dg-do compile } */
/* { dg-require-effective-target int128 } */

__int128 g_116_1;
extern char g_521[][8];
void func_24() {
  for (; g_116_1 >= 0;)
    g_521[g_116_1][g_116_1] &= 0;
}
