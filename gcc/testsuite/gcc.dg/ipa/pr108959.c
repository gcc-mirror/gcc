/* { dg-do compile } */
/* { dg-options "-O2" } */

union U2 {
  long f0;
  int f1;
};
int g_16;
int g_70[20];
static int func_61(int) {
  for (;;)
    g_70[g_16] = 4;
}
static int func_43(int *p_44)
{
  func_61(*p_44);
}
int main() {
  union U2 l_38 = {9};
  int *l_49 = (int *) &l_38;
  func_43(l_49);
}
