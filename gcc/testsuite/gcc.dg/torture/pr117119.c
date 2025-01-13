/* { dg-do compile } */
/* { dg-require-effective-target int128 } */

unsigned __int128 g_728;
int func_1_l_5011[8];
void func_1() {
  for (;; g_728 += 1)
    func_1_l_5011[g_728] ^= func_1_l_5011[g_728 + 5];
}
void main() {}
