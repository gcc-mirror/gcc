/* { dg-do compile } */
/* { dg-options "-O2" } */
#include <stdint.h>
int32_t g30;
void f25(int32_t a1)
{
    int a2;
    _Bool c6, c14;
    int8_t v8;
    int64_t v15;
    int8_t v16;
    if (a1 == 8098) goto lbl_sw11;
    a2 = __builtin_ffs(a1);
lbl_b8:
    v8 = a2;
    c6 = v8 >= v16;
    goto lbl_br26;
lbl_sw11:
lbl_br17:
    goto lbl_b8;
lbl_br26:
    g30 = v8;
    if (c6) __builtin_unreachable();
    c6 = c14 = a1;
    a1 = v8;
    if (v15) goto lbl_br26;
    if (c14) goto lbl_br17;
}
