/* { dg-do run } */
/* { dg-options "-O3 --param ipa-cp-eval-threshold=1"  } */

typedef int(*fp_t_5)(int, int, int);
int g2, g30, g21, g17, g16, g14, g9, g3, g12, g31;
_Bool g22, g26;
static
int f11(int, fp_t_5 a1, int, int, int)
{
    do {
      g12 = a1(1 ^ g14, 0, g9);
      if (g22)
        if (g2 != g17)
          if (g26)
            return g30;
    } while(true);
}
static __attribute__((noinline))
int f2(int a0, int, int a2)
{
    static int t = 0;
    t++;
    if (t > 2)
      __builtin_abort();
    g26 = true;
    g22 = true;
    if (g3 != a0) {
      g31 = 2;
      a0 = g31;
    }
    if (a0 == 2)
      if (a2 != 80)
        return 0;
    g17 = 6;
    g21 += f11(0, f2, 0, 0, 0);
    return 0;
}
void f1() { f2(0, 0, 0); }
int main()
{
    f2(0, 0, 0);
    return 0;
}
