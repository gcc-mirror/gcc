/* PR tree-optimization/127085 */
/* { dg-do compile } */

typedef unsigned char uint8_t;
typedef unsigned long long uint64_t;
uint8_t g26;
_Bool f19_c7, f19_c15;
void f19(uint64_t a0)
{
    _Bool c4;
lbl_sw3:
    if (f19_c7) goto lbl_bf30;
    goto lbl_cont19;
lbl_sw14:
    if (c4)
    lbl_cont19:
        if (f19_c15) goto lbl_sw14;
    goto lbl_sw3;
lbl_bf30:
    c4 = g26;
    if (a0 == 80504447930037642) goto lbl_sw14;
}
