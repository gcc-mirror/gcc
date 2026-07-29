/* { dg-do compile } */

int g6, g24;
_Bool f18_c6;
void f18()
{
    int a1;
    int bb16 = 607229;
lbl_sw5:
    bb16 = __builtin_clz(bb16);
    f18_c6 = a1;
    a1 = g24;
    if (f18_c6) goto lbl_sw5;
    g6 = __builtin_parity(bb16);
    return;
}
