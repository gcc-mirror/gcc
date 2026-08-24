/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <stdint.h>
typedef void (*fp_t_2)();
void *g12;
uint16_t g22;
_Bool f19_c11;
void f3()
{
lbl_dead2:
    goto lbl_dead2;
}
void f19()
{
    _Bool c0;
    fp_t_2 fp5;
    uint16_t v7;
    _Bool c10 = fp5 = f19;
lbl_sw1:
    if (c0) goto lbl_br5;
    if (f19_c11) goto lbl_br53;
    if (c10) __builtin_unreachable();
    return;
lbl_br5:
    c0 = 1 == g22;
    if (c0) goto lbl_sw_def26;
    if (c10) goto lbl_sw1;
lbl_bf10:
    c0 = 0 >= g22;
    c10 = g12;
    goto lbl_sw1;
lbl_sw_def26:
    fp5 = f3;
    g22 = v7;
    goto lbl_bf10;
lbl_br53:
    fp5();
    if (c10) __builtin_unreachable();
}
